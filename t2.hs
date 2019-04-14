import Text.Printf
import Data.Char
import System.IO

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]

greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0,80+i*10,0) | i <- [0..n] ]


-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [(((fromIntegral $ mod m 10)*(w+gap),(if fromIntegral m < 10 then h+50 else (((fromIntegral $ div m 10)*60)+(h+gap_x))  )),w,h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10
        gap_x = 50
-------------------------------------------------------------------------------
nCircles = 12
radius_from_center = 150

genCircle :: Int -> [Circle] --
genCircle n = [(((polarToCartesian_x cx m)+550,(polarToCartesian_y cy m)+250),r) | m <- [0..fromIntegral(n-1)]]
  where (cx,cy,r) = (60,60,30)
        angle_degrees :: Float -> Float
        angle_degrees m = degrees_separation nCircles *  m
      -- polarToCartesian_x cx radius_from_center
        polarToCartesian_x :: Float -> Float -> Float
        polarToCartesian_x cx m = cx + radius_from_center * cos (angle_radians m)

        polarToCartesian_y :: Float -> Float -> Float
        polarToCartesian_y cy m = cy + radius_from_center * sin (angle_radians m)

        angle_radians :: Float -> Float
        angle_radians m = angle_degrees m * ( pi / 180)
-------------------------------------------------------------------------------
-- calcula o grau de separação dos circulos
        degrees_separation :: Float -> Float
        degrees_separation nCircles=360/nCircles
-------------------------------------------------------------------------------
genCircleInter :: Int -> [Circle]
genCircleInter n = [(((cx_ (m+1) cx),cy_ (m+1) cy),r) | m <- [0..fromIntegral(n-1)]]
  where (cx,cy,r) = (100,100,30)
        gap = 10
cx_ :: Int -> Float -> Float
cx_ m cx
     |mod m 3 == 0 = cx + 175 + (fromIntegral m-2)*50
     |mod m 3 == 1 = cx + 200 + (fromIntegral m)*50
     |mod m 3 == 2 = cx + 150 + (fromIntegral m-1)*50

cy_:: Int -> Float -> Float
cy_ m cy
      |mod m 3 == 0 = cy + 175
      |mod m 3 == 1 = cy + 200
      |mod m 3 == 2 = cy + 200


--------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando circulo SVG
-- dadas coordenadas e dimensoes do circulo e uma string com atributos de estilo
svgCircle :: Circle -> String -> String
svgCircle ((cx,cy),r) style =
  printf "<circle cx='%.3f' cy='%.3f' r='%.2f' style='%s' />\n" cx cy r style
-- cx e cy p/ circle
-------------------------------------------------------------------------------
-- Gera string representando retângulo SVG
-- dadas coordenadas e dimensoes do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String
svgRect ((x,y),w,h) style =
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style
-- x e y p/ rect
--------------
-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' xmlns='http://www.w3.org/2000/svg'>\n" w h

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen; isolation: isolate; " r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma funcao geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles
-----------------------------------------------------------------------
genCase1 :: IO ()
genCase1 = do
writeFile "caseX.svg" $ svgstrs
 where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
       svgfigs = svgElements svgRect rects (map svgStyle palette)
       rects = genRectsInLine nrects
       palette = greenPalette nrects
       nrects = 30
       (w,h) = (1500,500) -- width,height da imagem SVG
------------------------------------------------------------------------
genCase2 :: IO ()
genCase2 = do
writeFile "caseX.svg" $ svgstrs
 where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
       svgfigs = svgElements svgCircle circles (map svgStyle palette)
       circles = genCircle ncircles
       palette = rgbPalette 12
       ncircles = 12
       (w,h) = (1500,500) -- width,height da imagem SVG
------------------------------------------------------------------------
genCase3 :: IO ()
genCase3 = do
writeFile "caseX.svg" $ svgstrs
 where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
       svgfigs = svgElements svgCircle circles (map svgStyle palette)
       circles = genCircleInter ncircles
       palette = rgbPalette 18
       ncircles = 18
       (w,h) = (1500,500) -- width,height da imagem SVG
-------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------

main :: IO ()
main = do
   putStr "Case 1,2 ou 3 : "
   num <-getLine
   if num == "1"
    then genCase1
    else if num == "2"
    then genCase2
    else if num == "3"
    then genCase3
    else return()
