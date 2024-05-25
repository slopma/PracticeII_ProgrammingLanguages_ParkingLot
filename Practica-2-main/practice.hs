import System.Directory
import Data.Time.Clock
import Data.List
import System.IO

-- Definición del tipo de datos para representar la info
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió

} deriving (Show, Read)  -- Agregamos Read aquí

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)

guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    -- Leer el contenido actual del archivo, si existe
    contenidoActual <- readFileIfExists "parqueadero.txt"
    -- Escribir el contenido actualizado en el archivo
    let info = mostrarVehiculo (parqueadero !! 0) ++ "\n"
    putStrLn contenidoActual
    writeFile "parqueadero.txt" (contenidoActual ++ info)
    putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para leer el contenido de un archivo si existe
readFileIfExists :: FilePath -> IO String
readFileIfExists path = do
    fileExists <- doesFileExist path
    if fileExists
        then readFile path
        else return ""

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    contenido <- readFile "parquedero.txt"
    let lineas = lines contenido
    return (map leerVehiculo lineas)
    where
        leerVehiculo linea = read linea :: Vehiculo

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    placa vehiculo ++ "," ++ show (entrada vehiculo) ++ "," ++ show (salida vehiculo)

-- Función para enlistar los vehículos
enlistarVehiculos :: [Vehiculo] -> IO ()
enlistarVehiculos parqueadero = do
    putStrLn "Vehículos en el parqueadero:"
    mapM_ (\v -> putStrLn $ mostrarVehiculo v) parqueadero

-- Función principal del programa
main :: IO ()
main = do

    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"

    -- Ciclo principal del programa
    cicloPrincipal parqueadero

-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Enlistar vehiculos"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            guardarParqueadero parqueaderoActualizado
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            cicloPrincipal parqueaderoActualizado
        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado
        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    let tiempoTotal = tiempoEnParqueadero vehiculo (entrada vehiculo)
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero
        "4" -> do
                    -- Enlistar vehículos
                    enlistarVehiculos parqueadero
                    cicloPrincipal parqueadero
        "5" -> putStrLn "¡Hasta luego!"
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero