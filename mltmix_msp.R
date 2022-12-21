#cargamos las librerias a utilizar
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(ggplot2)

#reiniciamos en entorno
rm(list=ls())

#' 1.- Crea un data frame o tibble a partir de los datos del fichero mktmix.csv. Usa la función clean_names() del paquete janitor para cambiar los nombres de columnas. A partir de ahora, haré referencia a este data frame como df_mmm, aunque lo puedes llamar como quieras.
df_mmm <- clean_names(read_csv("mktmix.csv"))

#Se trata de un fichero csv separado por ",", por lo que utilizamos la 
#función "read_csv" para la lectura del fichero y utilizamos la función "clean_names
#para tener todos los nombres de las columnas en minusculas y en el caso de que hubiera
#un espacio, tener en su lugar "_".


#' 2.- Cuántas columnas tiene el data frame? ¿Y filas? ¿Cuáles son las clases de las colum- nas base_price y discount? Explica qué información crees que contienen. **En este pregunta no evalúo el código que uses sino las respuestas que des a las preguntas.
glimpse(df_mmm)
#La funcion glimpse del paquete dplyr nos ofrece el nº de filas y columnas del dataframe , así como el nombre
#de las columnas, la clase de las mismas y los primeros valores de cada una de ellas. 

# - ¿Cuántas columnas tiene el data frame? 
#9

# - Y filas? 
#104

# - Cuáles son las clases de base_price y discount? 
#Double para ambas columnas

# - Explica qué información crees que contienen.
#base_price contiene el precio del producto mientras que discount presenta el porcentaje, 
#en el caso que lo tenga del producto. En el resultado de glimpse observamos valores de 0.00 y 0.05


#' 3.- La clase de newspaper_inserts es character. Cambia sus valores para que sea numérica de la siguiente forma: todos los valores NA deben ser 0; los demás, 1. Es verdad que pasarla a tipo logical haría que ocupara menos, pero como estás preparando datos para un modelo, es mejor que todo sea de tipo numérico.
unique(df_mmm$newspaper_inserts)
#Observamos mediante "unique" que la columna esta formada por NA e Insert

df_mmm <- df_mmm %>% 
  mutate(newspaper_inserts = if_else(is.na(newspaper_inserts),0,1))
#sobreescribimos el df, tras realizar un "mutate" con la condicion "if_else", la cual como condición
#incluimos "is.na", el cual si es NA nos da un valor "TRUE", por tanto lo sustimos por un "0", y en caso
#que la función arroje false, el valor sobreescrito será "1"

class(df_mmm$newspaper_inserts)
#comprobamos que la clase de este es numeric

#' 4.- ¿Cuántos valores distintos (o únicos) hay en la columna website_campaign (NA no cuenta)? Crea nuevas columnas para cada una de estas categorías, definidas como 1 si website_campaign toma el valor de esa categoría y 0 en caso contrario. Por ejemplo, si una de las categorías de website_campaign es "Google", crea una columna nueva lla- mada google que valga 1 en los registros en los que website_campaign valga "Google" y 0 en los demás. Recomendación. Los NA que hay en la columna original te pueden dar problemas a la hora de crear las nuevas columnas. Prueba a reemplazar esos NA por un valor auxiliar antes de crear las columnas.
unique(na.omit(df_mmm$website_campaign))

#al tratarse de un caracter especial y evitar problemas futuros cambiamos los NA por "NO"
df_mmm <- df_mmm %>% 
  mutate(website_campaign = replace_na(website_campaign,"NO"))

#Creamos una columna con los 1 para los valores Facebook y 0 para el resto, otra con twitter y otra con Website campaign.
#Para realizar esto creamos un codigo similar al realizado en el ejercicio 3, pero sin necesidad de utilizar la funcion is.na
df_mmm <- df_mmm %>% 
  mutate(facebook = if_else(website_campaign == "Facebook",1,0),
         twitter = if_else(website_campaign == "Twitter",1,0),
         website = if_else(website_campaign == "Website Campaign",1,0))

#' 5.- Cada fila de la tabla representa una semana en el histórico. Calcula cuántas semanas ha habido campaña de Facebook y cuántas semanas ha habido campaña de Twitter (hazlo con las columnas que calculaste en el ejercicio anterior).

sum(df_mmm$facebook)
sum(df_mmm$twitter)
#sumamos toda la variable ya que tiene 0 y 1, y obtenemos que ha habido 4 semanas de campaña tanto para 
#facebook como para twitter. 

#' 6.- La columna tv indica la cantidad de inversión que se ha hecho en anuncios de televisión. La unidad es grp. ¿Cuántas semanas se ha realizado una inversión de menos de 50 grp?

#aplicamos la funcion filter sobre tv, con la condicion que el valor sea menor que 50 y realizamos un nrow sobre toda la función. 
nrow(df_mmm %>% 
  filter(tv<50))

#' 7.- Calcula la media de inversión en tv durante las semanas en las que hubo inversión en radio y en durante las que no hubo (aquellas en las que radio sea NA). ¿Qué media es mayor? **Se valorará positivamente que realices este ejercicio con group_by() y summarise().
#La inversion media en tv cuando hay inversion en radio es de 137 vs los 171 cuando no hay inversion en radio

df_mmm %>% 
  mutate(radio1 = if_else(radio ==0 | is.na(radio),"no inversion","inversion")) %>% 
  group_by(radio1) %>% 
  summarise(mean(tv))

#creamos una variable provisional llamada radio1 la cual si existen inversion en radio (es decir si es mayor que 0 o diferente de NaN),
#tenemos valor "inversión" en caso contrario, "no inversión". Agrupamos por esta nueva variable con el group by, termiando obteniendo
#la media de la inversión en tv dependiendo si existe o no inversion en radio con la funcion "summarise(mean("")).
 
#' 8.- Crea un gráfico de líneas con ggplot2 que muestre la evolución de new_vol_sales, que es la columna con datos sobre las ventas del producto. Como no tienes datos de fechas, tendrás que inventarte el eje x: haz que sean valores 1, 2, 3, 4... hasta el número de filas. Pista. Puedes pasarle a ggplot directamente esos valores en el eje x, sin que sea una columna del data frame.

ggplot(df_mmm) +
  geom_line(aes(x=c(1:nrow(df_mmm)),y=new_vol_sales))+
  xlab("weeks")

#creamos el valor x de la grafica con un vector formado por el numero de semanas que es igual al numero de columnas que es el df
#incluimos este en los valores que nos pide la función geom_line, junto con new_vol_sales en el eje y.
#finalmente damos nombre a este ejer creado denominandolo "weeks".


#' 9.- Crea un histograma y un boxplot de esa variable, new_vol_sales. A ojo, a partir de los gráficos, ¿cuál dirías que es la mediana de la variable? Y sin hacer cálculos, ¿crees que la media es mayor o menor?

ggplot(df_mmm)+
  geom_histogram(aes(new_vol_sales), fill="darkolivegreen2", col="darkolivegreen")

#Creamos ambos graficos con la library ggplot2, en la cual primero indicamos el data frame a trabajar, y posteriormenrte la variable
# que queremos representar. en el caso de histograma lo respresenta contando cada vez que se repite un mismo nivel de ventas, hace un paquete de 30
#niveles de ventas diferentes. Esto podemos modificarlo indicando un numero diferente de bins:

ggplot(df_mmm)+
  geom_histogram(aes(new_vol_sales), bins=15, fill="darkolivegreen2", col="darkolivegreen")

#En el caso del boxplot, nos representa la cantidad de veces en proporcion que se repite un mismo nivel de ventas, pero además podemos observar, los quartiles 1 y 3,
#así como la mediana.

ggplot(df_mmm)+
  geom_boxplot( aes(new_vol_sales), fill="darkolivegreen2", col="darkolivegreen")

#Ambos graficos nos indican que existe una gran cola hacia volumenes más altos que hacia más bajos, por lo que podemos decir que la media es mayor que la mediana

#' 10.- Crea un data frame o tibble nuevo que tenga solo las columnas tv, radio y stout. Son las columnas que tienen datos de medios publicitarios: televisión, radio y exterior (carteles de esos que ves en la calle o en la carretera). Usa ese data frame para generar el gráfico siguiente. Usa el siguiente código también como paso previo antes de generar el gráfico. Cambiará el formato de tu nuevo data frame y te será más directo construir el gráfico. Para usar el código, he asumido que el nombre que le das al data frame nuevo es df_media pero puedes cambiar el nombre si quieres. Comentario. He quitado las etiquetas de los ejes para no darte pistas de cómo creo el eje x. Responde a lo siguiente: ¿qué merece la pena comentar a raíz del gráfico?

df_media <- df_mmm %>% 
  select(tv,radio, stout) %>% 
  pivot_longer(everything())

#Usa ese data frame para generar el gráfico siguiente.
ggplot(df_media, aes(x =1:nrow(df_media), y = value, xlab = "")) +
  geom_line() +
  xlab("") +
  ylab("") +
  facet_grid(name~ .,scales = "free_y",)

# Observamos que existe una mayor inversiñon en radio, auqnue como observamos y ya sabiamos de ejercicios anteriores, existen semanas
# sin inversión publicitaria en radio. En cambio televisión no tiene picos tan altos como los de radio, pero siempre existe inversión. 
#Finalmente en stout, la inversion no es matetial en comparación con las otras 2, representando entre un 1 y un 2% de la inversion en 
#radio o en tv. 


#' 11.- La columna in_store mide el stock disponible que hay en las tiendas para vender el producto, de manera indexada. Crea un gráfico de dispersión con ggplot que compare new_vol_sales frente a in_store. Presta atención a qué columna pondrás en el eje 𝑥 y cuál en el 𝑦: para ello, ten en cuenta que new_vol_sales será la variable objetivo del modelo, i.e., el analista explicar esa variable en función de las demás. Además, añade una capa con geom_smooth(): los ejes 𝑥 e 𝑦 serán los mismos que pongas en el geom del gráfico de dispersión. Comenta qué conclusiones sacas a la vista del gráfico (piensa en qué relación hay entre el stock y las ventas).

ggplot(df_mmm, aes(y =new_vol_sales, x = in_store)) +
  geom_point(shape=1) + 
  geom_smooth(alpha=0.3, fill="darkolivegreen2", col="darkolivegreen")

#observamos la tendencia por la cual a medida que existe mas stock, incrementan las ventas, auqnue 
#tampoco podemos concluirlo del todo, ya que no tenemos un gran numero de datos a partir de un stock > 50, 
#pero la linea de regeresión nos muestran que son variables directamente relacionadas,con pendiente positiva. 


#' 12.-  Repite el gráfico anterior pero de dos formas diferentes (no pongas geom_smooth esta vez).
#' • Colorea cada punto en función de la columna newspaper_inserts. Te recomiendo que uses as.factor() con esa columna en el gráfico.
#' • Colorea cada punto de manera diferente en función de la columna tv.

ggplot(df_mmm, aes(y =new_vol_sales, x = in_store)) +
  geom_point(aes(col= as.factor(newspaper_inserts)))+
  labs(col='newspaper_iserts') 

#dado el gran numero de datos, no utilizamos tv como factor debido al numero de datos diferentes que existen en la variable
ggplot(df_mmm, aes(y =new_vol_sales, x = in_store)) +
  geom_point(aes(col= tv))

#ambos graficos nos muestran que las ventas no se ven afectadas ni por la existencia de anuncios en los periodicos ni por el
#de inversión en televisión. 


#' 13.- Crea otra columna indicando si se ha aplicado descuento o no (es decr, si la columna discount es mayor que 0 o no). Puedes llamarla discount_yesno, por ejemplo, y puede ser numérica o logical. Luego agrega el data frame calculando la media de base_price para los casos en los que hay descuento y los casos en los que no. O sea, el resultado será un data frame de dos filas y dos columnas. Usa este data frame para crear un gráfico de columnas. Observación. Valoraré positivamente que no crees nuevos data frames (tampoco sobreescribas el origina porque lo necesitarás más adelante), sino que encadenes todas las operaciones con el operador pipe %>%, incluido el gráfico de ggplot.

df_mmm %>% 
  mutate(df_mmm, discount_yesno = ifelse( discount > 0, T, F)) %>% 
  group_by(discount_yesno) %>% 
  summarise(media_desc = mean(base_price)) %>% 
  ggplot(aes(  x= as.factor(discount_yesno), y = media_desc )) +
  geom_col(fill="darkolivegreen", alpha=0.8)+
  xlab("discount") +
  ylab("mean base price")

#Observamos un precio base medio menor en las entradas con descuento pero no significativo.


#' 14.- Apóyate en el siguiente código para crear una función que ajuste un modelo de regresión en los datos. La función recibirá como entrada un vector character con las variables que se quieran usar como explicativas en el modelo. Con ese vector, dentro de la función crea un nuevo data frame que tenga esas variables y la variable explicativa, new_vol_sales. Este data frame lo usarás para ajustar el modelo siguiendo el código siguiente. Ese código calcula lo que se llama R cuadrado ajustado, una métrica qué indica cómo de bueno es el modelo: cuanto más alto, menor error hay. Finalmente, llama a la función usando como vector de entrada c("tv", "radio"), es decir, ajustarás un modelo que intente explicar las ventas en función de la publicidad realizada en televisión y en radio. Observación. Lo que en el código se llama df_aux hace referencia al data frame auxiliar que crearás dentro de la función.

f14 <- function(variables){
  
  condition <- sum(as.numeric(variables %in% names(df_mmm))) == length(variables)

  if(condition){
    df_aux <- df_mmm %>% 
      select(c(new_vol_sales, variables))
    my_model <- lm(new_vol_sales ~ ., data = df_aux)
    
    return(summary(my_model)$adj.r.squared)
  } else{("Variable incorrecta")}
}

f14(c("tv","radio"))

f14(c("tv","radio","internet"))

#creamos una primera condición a través de la cual comprobamos si las variables seleccionadas,
#están dentro del data frame y que son numericos. Posteriormente, si esta condición se cumple, creamos el 
#df auxiliar incluyendo unicamente las variables seleccionadas y la explicativa. Generamos un nuevo df, 
#siendo una regresion linear de la variable explicativa y las seleccionadas. A la función le pedimos que 
#el return sea unicamente el r2 de este ultimo df creado. En en caso de que las variables no sean validas,
#la funcón nos dará un mensaje de "variable incorrecta"


#' 15.- Debajo tienes tres vectores con nombres de variables. Crea una lista con esos vectores, es decir, una lista de tres elementos (cada elemento será cada uno de los vectores). Ahora usa map_dbl() o sapply() para llamar a tu función para cada uno de los vectores. El resultado de tu código será un vector de tres números: cada número será el R cuadradado ajustado de los modelos que se ajustan con esos conjuntos de variables. ¿Qué modelo es el mejor, de acuerdo al R cuadrado ajustado?
#' • c("base_price", "radio", "tv", "stout")
#' • c("base_price", "in_store", "discount", "radio", "tv", "stout") 
#' • c("in_store", "discount")

m1 <- c("base_price", "radio", "tv", "stout")
m2 <- c("base_price", "in_store", "discount", "radio", "tv", "stout")
m3 <- c("in_store", "discount")

listam <- list(M1=m1,M2=m2,M3=m3)

sort(sapply(listam,f14), decreasing = TRUE)

#creamos una lista que incluya los tres modelos (los cuales tienen un formato vecor). A esra lista le aplicamos la formula
#del ejercicio 14 "f14" y ordenamos los resultados de esta en order decreciente, donde observamos que el modelo 2 es que tiene 
#el mayor alor de r2 el que mejor explicaría la variable "new_vol_sales"






