{-# LANGUAGE FlexibleInstances #-}
import Data.Complex
import Data.Numeric.Function
import Graphics.Gnuplot.Simple

-- the physical constant are too small and creates rounding errors
l=1.0e-11
h=6.62606957e-34
m=9.10938188e-31
ts = [0.00,0.01e-19..0.60e-19]

e :: Complex Double -> Complex Double
e n = n^2 * h^2 * pi^2 / (2 * m * l^2)

k :: Complex Double -> Complex Double
k n = n*pi/l

w :: Complex Double -> Complex Double
w n = (e n)/h

a :: Complex Double
a = sqrt(2/l)

img :: Complex Double
img = 0 :+ 1

eexp :: Complex Double -> Complex Double
eexp c = (mkComp (2.7182818**(realPart c))) * ((cos (imagPart c)) :+ (sin (imagPart c)))

psi :: Complex Double -> Double -> Complex Double -> Complex Double
psi n t x = a * sin((k n) * x) * (eexp (-img*(w n)*(t:+0)))

nums = map (\x -> x :+ 0) [1..3]

psis = (1/(sqrt . fromIntegral $ (length nums))) * sum $ map psi nums

sans = (^2) . psis

mkComp x = x :+ 0

y :: [[[Double]]]
y = map (\gress -> [map magnitude gress, map realPart gress, map imagPart gress]) funcs
  where xs = map mkComp (linearScale 300 (0, realPart l))
        funcs :: [[Complex Double]]
        funcs = map (\t -> map (sans t) xs) ts

-- | dirty string format
gress :: Int -> String
gress i
  | i < 10 = "00" ++ show i
  | i < 100 = "0" ++ show i
  | otherwise = show i

onePlot :: [[Double]] -> Int -> IO ()
--onePlot ys i = plotLists ([PNG ("/tmp/kvantePNG" ++ gress i ++ ".png")]) ys
onePlot ys i = plotLists ([YRange (-5.0e11,5.0e11), PNG ("/tmp/kvantePNG" ++ gress i ++ ".png")]) ys
--onePlot ys i = plotLists ([YRange (-3,3), PNG ("/tmp/kvantePNG" ++ gress i ++ ".png")]) ys

fun :: (Int, [[Double]]) -> IO ()
fun (i,ys) = onePlot ys i

jazz :: [(Int, [[Double]])]
jazz = zip [1..] y

main = mapM fun jazz
