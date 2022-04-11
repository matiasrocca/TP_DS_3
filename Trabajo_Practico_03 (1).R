rm(list=ls())
setwd() # Definir directorio de trabajo.

#install.packages("ROCR")
library(ROCR) #para medir la preformance de nuestro modelo
#library(e1071) #para correr el ejemplo (nb)

library(rpart) #para arboles
library(rpart.plot) #para graficarlos

#~~ Introducción a Ciencia de Datos: Taller 03 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fuente de los datos: https://www.kaggle.com/adammaus/predicting-churn-for-bank-customers

# Definición de funciones:

# Función para cargar los datos
load_data <- function(data_path) {
    # data_path: debe ser el path en donde se encuentra el archivo con los datos
    train_data <- read.table(data_path, sep=",", header=TRUE, quote="")
    train_data$Exited <- factor(ifelse(train_data$Exited == 1, "churn", "no_churn"))
    train_data$RowNumber <- NULL
    train_data$Surname <- NULL
    return(train_data)
}

# Función que permite calcular el AUC
get_auc <- function(real_class, predicted_prob) {
    # real_class: debe ser un vector de 0 y 1, en donde 1 es la clase de interés
    # predicted_prob: de ser un vector de probabilidades predichas para la clase de interés
    pred_ROCR <- prediction(predicted_prob, real_class)
    auc_ROCR <- performance(pred_ROCR, measure = "auc")
    auc_ROCR <- auc_ROCR@y.values[[1]]
    return(auc_ROCR)
}

# Resolución de ejercicios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

## Ejercicio 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
churn_data= load_data("Churn_Modelling.csv")

folds <- 10
indexes <- sample(rep_len((1:folds), nrow(churn_data)))
tree_vld_accuracy <- data.frame()

for (f in c(1:folds)) {
  
  f
  train_data <- churn_data[indexes!=f,]
  val_data  <- churn_data[indexes==f,]
  
  tmp_tree_model <- rpart(Exited ~ ., data = train_data)
  tmp_tree_preds <- predict(tmp_tree_model, newdata=val_data, type = "prob")[, "churn"]
  tree_vld_accuracy  <- rbind(tree_vld_accuracy,
                            data.frame(fold = f,
                                       auc = get_auc(as.numeric(val_data$Exited == "churn"), tmp_tree_preds)))
}
mean(tree_vld_accuracy$auc)

"No utilizamos ningún control de hiperparametro. Simplemente corremos en 10 
folds distintos un mismo modelo (pero que usa distintos datos para
entrenarse y testear) y determinar cual es el acierto promedio (es decir el 
promedio de los 10 folds, que da justamente, el AUC score promedio del modelo
para datos nuevos).

Para lograr eso entrenamos un modelo con distintos datos, le pedimos la
predicción (y la probabilidad de que algo sea o no churn), y testeamos que tan 
bien ese modelo acertó (con esos datos de entrenamiento y testeo) la predicción 
con la realidad."


## Ejercicio 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
indice_sample_valid= sample(c(1:nrow(churn_data)), 1000)
validation_set= churn_data[indice_sample_valid,]

## Ejercicio 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
"usamos setdiff(c(1:nrow(churn_data)), indice_sample_valid) para eliminar de
churn los índices del validation set y creamos una muestra a partir de eso"
indice_sample_test= sample(setdiff(c(1:nrow(churn_data)), indice_sample_valid), 1000)
test_set= churn_data[indice_sample_test,]

## Ejercicio 4 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
training_set= churn_data[-c(indice_sample_valid, indice_sample_test),]

## Ejercicio 5 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rpart_exp= data.frame()
vect= c(1:10)
vect2= c(1:10)
vect3= c(1:10)
for (numero in vect) {
  for (numero2 in vect2) {
    for (numero3 in vect3) {
      arbol_ej5= rpart(Exited ~ ., data = training_set,
                       control = rpart.control(minsplit= numero, minbucket= numero2, maxdepth= numero3, xval=0, cp=0))
      predicc_tr= predict(arbol_ej5, newdata= training_set, type = "prob")[, "churn"]
      predicc_vld= predict(arbol_ej5, newdata= validation_set, type = "prob")[, "churn"]
      rpart_exp= rbind(rpart_exp, data.frame(minsplit=numero, minbucket=numero2, maxdepth=numero3, auc_tr= get_auc(as.numeric(training_set$Exited == "churn"), predicc_tr), auc_vld= get_auc(as.numeric(validation_set$Exited == "churn"), predicc_vld)))
    }
  }
}

## Ejercicio 6 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
"Dado el ej anterior, usamos la siguiente linea para averiguar cual de los 1000
modelos preoformó mejor (y tuvo la combinación idonea de hiperp.):"
which.max(rpart_exp[,5])
#para guardar eso, usamos:
hiperp_modelo_idoneo= rpart_exp[which.max(rpart_exp[,5]),]
hiperp_modelo_idoneo
"Esto hace que se actualicen el valor de los mejores hiperparametros en caso de 
cambiar las sample del ejercicio 5, permitiendonos referirnos siempre al valor 
de los mejores hiperparametros del mejor modelo entre los 1000 posibles"

## Ejercicio 7 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
train_val_sets= churn_data[-indice_sample_test,]
mejor_arbol= rpart(Exited ~ ., data= train_val_sets,
                              control= rpart.control(minsplit= hiperp_modelo_idoneo[,1], minbucket= hiperp_modelo_idoneo[,2], maxdepth= hiperp_modelo_idoneo[,3], xval=0, cp=0))

## Ejercicio 8 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
predicc_mejor_arbol= predict(mejor_arbol, newdata= test_set, type = "prob")[, "churn"]
auc_mejor_arbol= get_auc(as.numeric(test_set$Exited == "churn"), predicc_mejor_arbol)
auc_mejor_arbol

## Ejercicio 9 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rpart.plot(mejor_arbol)

## Ejercicio 10 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mejor_arbol$variable.importance
"Considerando que churn es el momento a partir del cual un cliente deja de hacer 
negocios, parecería lógico pensar que la edad sea la variable que más relevancia
tiene. El resto de los cortes del arbol, a su vez, también parecerían ser
razonables. El numero de productos, el balance, la actividad del miembro, y su 
localización geografica también son determinantes."

"Lo que resulta un poco más extraño, es que la profundidad ideal del arbol sea 
tan alta, particionando mucho al arbol."

