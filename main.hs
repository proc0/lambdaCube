module Main where

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Colour (withOpacity) 
import Data.List.Split
import Data.Maybe
import Data.Tuple.All

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude 
import Diagrams.TwoD
import Diagrams.BoundingBox
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont

type Cell = QDiagram B V2 Double Any
type Font = PreparedFont Double

cellPad :: Double
cellPad = 0.8

lineCell :: Cell -> Double -> Cell
lineCell content space = centered <> shape
    where 
        spaceY = strutY space
        spaceX = strutX space
        centered = centerXY content
        spaced = 
            spaceY 
            === 
            (spaceX ||| centered ||| spaceX)
            === 
            spaceY
        height = diameter up spaced
        boxColor = fcA $ green `withOpacity` 0.5
        shape = roundedRect height height 0.3 
              # lw 1 
              # boxColor

lineBox :: String -> Cell -> Cell
lineBox name content = lineCell content cellPad 
                     # named name

textCell :: Cell -> Double -> Cell
textCell content space = centered <> shape
    where 
        spaceY = strutY space
        spaceX = strutX space
        centered = centerXY content
        spaced = 
            spaceY 
            === 
            (spaceX ||| centered ||| spaceX)
            === 
            spaceY
        height = diameter (r2 (0,1)) spaced
        shape = roundedRect height height 0.3 
              # lw 0


textBox :: String -> Cell -> Cell
textBox name content = textCell content cellPad 
                     # named name

idle :: V2 Double
idle = r2 (0, 0)

up :: V2 Double
up = r2 (0, 1)

upright :: V2 Double
upright = r2 (0.7, 0.5)

right :: V2 Double
right = r2 (1, 0)

downright :: V2 Double
downright = r2 (0.7, -0.5)

down :: V2 Double
down = r2 (0, -0.5)

downleft :: V2 Double
downleft = r2 (-0.5, -0.5)

left :: V2 Double
left = r2 (-0.5, 0)

upleft :: V2 Double
upleft = r2 (-0.5, 0.7)

stepSize :: V2 Double
stepSize = r2 (15, 13)

move :: String -> Cell -> Cell
move trail = translate . step $ hike trail
    where
        step :: [V2 Double] -> V2 Double
        step = foldl (\b a -> a*stepSize ^+^ b) idle
        directions :: Char -> V2 Double
        directions d = case d of 
            '↑' -> up
            '🡕' -> upright
            '→' -> right
            '🡖' -> downright
            '↓' -> down
            '🡗' -> downleft
            '←' -> left
            '🡔' -> upleft
            _   -> idle
        hike :: String -> [V2 Double]
        hike = map directions

textSize :: Double
textSize = 2

cubeSpec :: [(String, String, String)]
cubeSpec = [ ("box1", "λ", "")
           , ("box2", "λP", "→")
           , ("box3", "λω", "🡕")
           , ("box4", "λ2", "↑")
           , ("box5", "λP2", "→↑")
           , ("box6", "λω", "↑🡕")
           , ("box7", "λPω", "🡕→")
           , ("box8", "λC", "↑→🡕")
           ]

getBoxes :: (Double -> String -> Cell) -> Diagram B
getBoxes mkLabel = mconcat $ mkBoxes cubeSpec
    where 
        newBox :: String -> String -> String -> Cell
        newBox name label trail = textBox name boxLabel  
            where 
                isBehind = any (== '🡕') trail
                boxFontSize = 
                    if isBehind 
                    then textSize*0.75
                    else textSize
                boxLabel = mkLabel boxFontSize label

        mkBox :: String -> String -> String -> Cell
        mkBox name label trail = 
            if not . null $ trail
            then box # move trail
            else box
            where box = newBox name label trail

        mkBoxes :: [(String, String, String)] -> [Cell]
        mkBoxes = map $ uncurryN mkBox

cubeHOM :: [(String, String)]
cubeHOM = [ ("box7","box8")
          , ("box6","box8")
          , ("box5","box8")
          , ("box4","box6")
          , ("box4","box5")
          , ("box3","box7")
          , ("box3","box6")
          , ("box2","box7")
          , ("box2","box5")
          , ("box1","box4")
          , ("box1","box3")
          , ("box1","box2")
          ]

getGraph :: Diagram B -> Diagram B
getGraph = foldr (.) id arrows
    where 
        arrows = map drawArrow cubeHOM
        drawArrow = uncurry $ connectOutside' arrowOpts
        arrowOpts = with
                  & headLength .~ small
                  & shaftStyle %~ lw thin

getDiagram :: (Double -> String -> Cell) -> Diagram B
getDiagram mkLabel = pad 1.1 $ centerXY diagram <> frame
    where diagram = boxes <> getGraph boxes
          boxes = getBoxes mkLabel
          frame = square 34
                # fc whitesmoke

fontLabel :: Font -> Double -> String -> Cell
fontLabel font fs label = textSVG_ textOpts label # fc black # lw none
    where textOpts = TextOpts font INSIDE_H KERN False 1 fs

textWrap :: Font -> Double -> [String] -> Cell
textWrap font fs ls = vcat' textOpts $ textList ls
    where textOpts = with 
                   & catMethod .~ Distrib 
                   & sep .~ fs
          textList = map $ centerX . fontLabel font fs

getLabel :: Font -> Double -> String -> Cell
getLabel font fs label = textWrap font fs $ splitOn "\n" label

main :: IO ()
main = do
    font <- lin
    let mkLabel fs label = getLabel font fs label
    mainWith $ getDiagram mkLabel
    