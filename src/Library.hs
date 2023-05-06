module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

palos = ["Corazones", "Picas", "Tréboles", "Diamantes"]
type Carta = (Number, String)
type Cartas = [Carta]
-- En el caso de los jugadores, se constituyen por: su nombre, 
-- la mano que están jugando y el nombre de su bebida preferida.

data Jugador = Jugador {
    nombre :: String,
    mano :: Cartas,
    bebida :: String
} deriving Show
{- 
James Bond (el bueno), cuyo nombre es "Bond... James Bond", tiene un poker de ases y toma "Martini... shaken, not stirred".
Le Chiffre (el malo), que tiene un full de jokers y toma Gin.
Felix Leiter, que tiene una pierna de nueves. Su bebida preferida es el Whisky.
-}
jamesBond = Jugador "Bond... James Bond" pokerDeAses  "Martini... shaken, not stirred"
pokerDeAses    = [(1,"Corazones"), (1,"Picas"), (1,"Tréboles"), (1,"Diamantes"), (10,"Diamantes")]

leChiffre = Jugador "Le Chiffre" fullDeJokers "Gin"
fullDeJokers   = [(11,"Corazones"), (11,"Picas"), (11,"Tréboles"), (10,"Diamantes"), (10,"Picas")]

felixLeiter = Jugador "Felix Leiter" piernaDeNueves "Whisky"
piernaDeNueves = [(9,"Corazones"), (9,"Picas"), (9,"Tréboles"), (10,"Diamantes"), (4,"Copas")]

mesaQueMasAplauda = [jamesBond, leChiffre, felixLeiter]

--1.a
mayorSegun f valor1 valor2
    | f valor1 > f valor2 = valor1
    | otherwise = valor2

--1.b
maximoSegun _ [x] = x 
maximoSegun f (x:y:xs) = maximoSegun f (mayorSegun f x y  : xs)

maximoSegun' f = foldl1 (mayorSegun f) 
--maximoSegun f (x:y:xs) 
--   | mayorSegun f x y == x = maximoSegun f (x:xs)
--  | otherwise = maximoSegun f (y:xs)

--1.c
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/=x) xs)
    

--2.a
esoNoSeVale = not.esoSeVale
esoSeVale (numero, palo) = elem numero [1..13] && elem palo palos

--2.b
--manoNegra jugador = ((/=5).length.mano) jugador || (any esoNoSeVale.mano $ jugador)
--($) f p = f p
manoNegra (Jugador _ cartas _) = ((/=5).length) cartas || any esoNoSeVale cartas


ocurrenciasDe x = length . filter (== x)
--3.a
numero = fst
palo = snd
par = aparece 2

aparece n cartas =  any ((n==).flip ocurrenciasDe numeros) [1..13]
    where numeros = map numero cartas

aparece' n cartas =  any (\num -> n == ocurrenciasDe num numeros) [1..13]
    where numeros = map numero cartas
--flip f a b = f b a
--3.b
pierna = aparece 3
--3.c
color :: [Carta] -> Bool
color (carta:cartas) = all ((==palo carta).palo) cartas
--3.d
fullHouse cartas = par cartas && pierna cartas
fullHouse' = flip all [par, pierna].flip ($)
--3.e
poker = aparece 4
--3.f
otro _ = True

--4
alguienSeCarteo mesa = sinRepetidos todasLasCartas /= todasLasCartas
    where todasLasCartas = concat . map mano $ mesa

--5.a
valores = [(par,1), (pierna,2), (color,3), (fullHouse,4), (poker,5), (otro, 0)]

valor mano = snd . maximoSegun snd . filter (($mano).fst) $ valores

--5.b
bebidaWinner = bebida . maximoSegun (valor.mano) . filter (not.manoNegra)

--6
--a
--El nombre del jugador que está tomando la bebida de nombre más largo.
-- > nombre.maximoSegun (length.bebida) $ mesaQueMasAplauda


--7.a
ordenar _ [] = []
ordenar criterio (x:xs) =
    anteriores ++ [x] ++ posteriores
    where aplica param = ordenar criterio (filter (param.criterio x) xs)
          anteriores = aplica not
          posteriores = aplica id

--7.b
escalera mano = numerosOrdenados == [head numerosOrdenados.. head numerosOrdenados + 4] 
        where numerosOrdenados = ordenar (<) . map numero $ mano


escaleraDeColor mano = escalera mano && color mano








