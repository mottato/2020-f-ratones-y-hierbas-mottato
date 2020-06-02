module Lib where
import Text.Show.Functions

--1)
type Nombre =String
type Edad=Float
type Peso=Float
type Enfermedades = [String]
type Enfermedad= String

data Raton = Raton {
    nombre :: Nombre,
    edad::Edad,
    peso :: Peso,
    enfermedades :: Enfermedades
} deriving (Show,Eq)

cerebro = Raton "Cerebro" 9 0.2 ["brucelosis", "sarampion" ,"tuberculosis"]
bicenterrata= Raton "Bicenterrata" 256 0.2 []
huesudo = Raton "Huesudo" 4 10 ["alta obesidad", "sinusitis"]


--2)
type Hierba = Raton ->Raton

hierbaBuena :: Hierba
hierbaBuena unRaton = modificarEdad sqrt unRaton

modificarEdad :: (Edad->Edad)->Raton->Raton
modificarEdad unaFuncion unRaton = unRaton{
    edad = unaFuncion.edad $unRaton
} 

modificarPeso :: (Peso->Peso)->Raton->Raton
modificarPeso unaFuncion unRaton =unRaton {
    peso = unaFuncion.peso $unRaton
}

modificarEnfermedad :: (Enfermedades->Enfermedades)->Raton->Raton
modificarEnfermedad unaFuncion unRaton =unRaton {
    enfermedades = unaFuncion.enfermedades $unRaton
}

enfermedadesQueNoterminanDeCiertaForma :: String->Enfermedades->Enfermedades
enfermedadesQueNoterminanDeCiertaForma terminacion = filter (not.(== terminacion).tail) 


hierbaVerde :: String->Hierba
hierbaVerde terminacion unRaton= modificarEnfermedad (enfermedadesQueNoterminanDeCiertaForma terminacion) unRaton

perderPeso :: Peso ->Peso
perderPeso unPeso
 |(>2) unPeso = (restarPeso 10) unPeso
 | otherwise =(restarPeso 5) unPeso


restarPeso :: Float->Float->Float
restarPeso porcentajeAPerder unPeso = unPeso - ((*(porcentajeAPerder/100)) unPeso)

alcachofa :: Hierba
alcachofa unRaton = modificarPeso perderPeso unRaton

hierbaZoft :: Hierba
hierbaZoft  = modificarEdad (\edad->0).transformarseEnPinky.modificarEnfermedad (\enfermedades->[])

transformarseEnPinky :: Raton->Raton
transformarseEnPinky unRaton = unRaton {nombre = "Pinky" }


enfermedadesConMasDe10Letras :: Enfermedades->Enfermedades
enfermedadesConMasDe10Letras = filter (\enfermedad-> (>10).length $enfermedad) 

hierbaDelDiablo:: Hierba
hierbaDelDiablo= (modificarPeso perderPesoDiablo).(modificarEnfermedad enfermedadesConMasDe10Letras)
 
perderPesoDiablo :: Peso->Peso
perderPesoDiablo unPeso= max 0 (unPeso-0.1*unPeso)


--3)
type Medicamento= Raton->Raton
pondsAntiAge :: Medicamento
pondsAntiAge =hierbaBuena.hierbaBuena.hierbaBuena.alcachofa

exponenteAlcachofa:: Int->(Medicamento)
exponenteAlcachofa 1 = alcachofa
exponenteAlcachofa unaPotencia = alcachofa.(exponenteAlcachofa (unaPotencia-1))

reduceFatFast :: String->Int->Medicamento
reduceFatFast terminacion unaPotencia unRaton=(exponenteAlcachofa unaPotencia).(hierbaVerde terminacion) $unRaton

sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

pdepCilina:: Medicamento
pdepCilina unRaton = foldr hierbaVerde  unRaton sufijosInfecciosas

--4)

cantidadIdeal :: (Int->Bool)->Int
cantidadIdeal unaCondicion = primeroQueCumple unaCondicion 

primeroQueCumple :: (Int->Bool)->Int
primeroQueCumple unaCondicion = head (filter unaCondicion [1..]) 

lograEstabilizar :: [Raton]->Medicamento->Bool
lograEstabilizar comunidadRatones unMedicamento= ningunoTieneSobrePeso comunidadRatones unMedicamento && todosTienenMenosDeTresEnfermedades comunidadRatones unMedicamento

aplicarMedicamento :: [Raton]->Medicamento->[Raton]
aplicarMedicamento comunidadRatones unMedicamento= map unMedicamento comunidadRatones

ningunoTieneSobrePeso :: [Raton]->Medicamento->Bool 
ningunoTieneSobrePeso comunidadRatones unMedicamento= all (noTieneSobrepeso) (map peso (aplicarMedicamento comunidadRatones unMedicamento))

noTieneSobrepeso :: Peso->Bool
noTieneSobrepeso = not.(>1)

todosTienenMenosDeTresEnfermedades :: [Raton]->Medicamento->Bool 
todosTienenMenosDeTresEnfermedades comunidadRatones unMedicamento = all (tienenAlMenosTresEnfermedades) (map enfermedades (aplicarMedicamento comunidadRatones unMedicamento))

tienenAlMenosTresEnfermedades :: Enfermedades->Bool
tienenAlMenosTresEnfermedades  = (<=3).length

potenciasIdeales :: [Raton]->String->Int
potenciasIdeales comunidadRatones terminacion
 |lograEstabilizar comunidadRatones (alcachofa.(hierbaVerde terminacion)) = 1
 |otherwise= 1 + potenciasIdeales comunidadRatones terminacion


 --5
 {-
 a) En el caso de que todos pesen menos de 1kg y no tengan enfermedades, Haskell jamas terminará de dar una
 respuesta, ya que la utiliza un all, la cual buscará hasta finalizar la lista si todos cumplen con la 
 condicion dada. Por lo tanto jamás se sabrá si todos los ratones realmente cumplen. Por lo tanto no devolvera
 nada.
 b) En caso de que un Raton pese 2kg y tenga 4 enfermedades, Haskell devolverá False, ya que si bien la lista
 es infinita, dejará de evaluarla cuando encuentre un elemento que no cumpla con la condicion dada, puesto que
 al estar definida con un all, implica que todos los elementos cumplan.
 -}

 --6
 {-
 a) No habría que modificar las funciones existentes ya que la funcion lograEstabilizar recibe una funcion
 por parametro y eso hace que se le pueda aplicar al raton el medicamento que se desee sin importar la cantidad
 de parametros que reciba.
 b)Los conceptos que se utlizan en el punto anterior es el de orden superior ya que la funcion recibe como
 uno de sus parametros una funcion lo que ayuda a que sea mas generica posible. Ademas en el caso de que una
 funcion requiera mas de un parametro, en el momento de pasarla por parametro se aplica parcialmente con el
 parametro faltante. Por ejemplo lograEstabilizar [cerebro,huesudo] (hierbaVerde terminacion), donde terminacion
 no es un parametro de la funcion lograEstabilizar y la terminacion se define en la consulta
 c) En el caso de que el peso de los ratones se quiera registrar en libras no se modificaria ninguna funcion,
 puesto que el tipo de dato definido en si no influye y ademas si se desea agregarle otro campo al data en
 ningun momento se descompone en las funciones. Por lo tanto nada modificaria.
 -}