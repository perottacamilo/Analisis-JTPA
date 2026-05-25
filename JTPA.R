library(tidyverse)    # Para el manejo de variables
library(ggplot2)      # Está incluído en tidyverse (para hacer gráficos)
library(dplyr)        # Está incluído en tidyverse (para el manejo de variables)
library(haven)        # Para importar archivos de tipo .dta o .sav  
library(readxl)       # Para importar archivos de excel
library(writexl)      # Para exportar archivos de excel
library(skimr)        # Genera resúmenes estadísticos más completos que
library(janitor)      # Limpieza de nombre de ariables
library(broom)        # Convierte resultados de modelos en data frames ordenados.
library(sandwich)     # Genera errores estandar
library(lmtest)       # Sirve para tests estadísticos para modelos lineales (heter., autocorr., etc.).
library(modelsummary) # Formatos más visibles de regresiones
holahola
#Clase 1
library(AER)
library(lmtest)


rm(list = ls())

  setwd("D:/Users/Camilo/Documents/.-Camilo-/UBA/Econometria I/TPs") 
jtpa_tp <- read_dta("jtpa.dta")

jtpa_tp <- jtpa_tp %>% 
  mutate(ingreso = as.numeric(earnings), secund_compl = as.numeric(hsorged))
  
jtpa_2 <- jtpa_tp %>% 
  filter(ingreso > 0) %>% 
  mutate(log_ingreso = log(ingreso))


#Trabajo Practico 2 (Job Training Partnership Act)

#Punto 1
#Regresion de Earning con JTPA training

#Modelo en niveles (dolares)

#Modelo Lin-Lin
ejercicio1 <- lm(ingreso ~ jtpa_training, data = jtpa_2)
summary(ejercicio1)

#Modelo Log-Lin
ejercicio1.1 <- lm(log_ingreso ~ jtpa_training, data = jtpa_2)
summary(ejercicio1.1)


#Punto 2
#Evaluar si existen diferencias entre hombres (0) y mujeres (1)

# Incorporamos características observables del individuo
# que también afectan los ingresos y pueden estar correlacionadas
# con haber participado en la capacitación

ejercicio2 <- lm(log_ingreso ~ jtpa_training + sex + jtpa_training*sex + secund_compl + black 
+ hispanic + married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_2)
summary(ejercicio2)

linearHypothesis(ejercicio2, "jtpa_training:sex = 0")

#Endogeneidad de jtpa_training
#Es endogena si Cov(jtpa_training;error)≠0
#Hay una variable que no aparece en el modelo y que esta relacionada con jtpa_training

#Prueba de Endogeneidad. Auto seleccion: Si bien la oferta al curso fue aleatoria
#Cada persona elegia si asistir o no
#Por lo que podria ocurrir que asistan aquellas mas motivadas o con mas capacidades o caract. no obs.
#Utilizar MCO implica comparar aquellos que se capacitaron contra los que no, algunos eliminados por azar y otros auto-elimnados
#Pero estan incluidos tantos los que no recibieron la oferta (Z=0), como los que si (Z=1) y la rechazaron (D=0)
#Como rechazar la oferta no esta relacionado con el azar, probablemente rechazaron los menos motivados (no observable)
#Como rechazar o no esta relacionado con variables no observables se genera la endogeneidad
#Al utilizar la oferta como Variable Instrumental de la participación real, que se muestra que fue aleatoria
#Se elimina cualquier relacion (Cov) con variables no observables, mostrando exogeneidad



#Exogeneidad
#Ver que la variable instrumental jtpa_offer no esta relacionada con los errores
#Una manera de mostrarlo es ver que la oferta al programa fue de manera aleatoria
table(jtpa_2$jtpa_offer, jtpa_2$jtpa_training)
cor(jtpa_2$jtpa_offer, jtpa_2$jtpa_training)


balance <- data.frame(
  Variable = c("secund_completo", "black", "hispanic",
               "married", "wkless13", "sex"),
  No_asignado = round(c(
    mean(jtpa_2$secund_compl[jtpa_2$jtpa_offer == 0]),
    mean(jtpa_2$black[jtpa_2$jtpa_offer == 0]),
    mean(jtpa_2$hispanic[jtpa_2$jtpa_offer == 0]),
    mean(jtpa_2$married[jtpa_2$jtpa_offer == 0]),
    mean(jtpa_2$wkless13[jtpa_2$jtpa_offer == 0]),
    mean(jtpa_2$sex[jtpa_2$jtpa_offer == 0])
  ), 3),
  Asignado = round(c(
    mean(jtpa_2$secund_compl[jtpa_2$jtpa_offer == 1]),
    mean(jtpa_2$black[jtpa_2$jtpa_offer == 1]),
    mean(jtpa_2$hispanic[jtpa_2$jtpa_offer == 1]),
    mean(jtpa_2$married[jtpa_2$jtpa_offer == 1]),
    mean(jtpa_2$wkless13[jtpa_2$jtpa_offer == 1]),
    mean(jtpa_2$sex[jtpa_2$jtpa_offer == 1])
  ), 3)
)
print(balance)




#Ver si jtpa_offer esta relacionado con los residuos de la regresion
residuo <- (ejercicio2$residuals)
exogeneidad <- lm(residuo ~ jtpa_offer, data = jtpa_2)
summary(exogeneidad)

#Ver si jtpa_offer esta relacionada con los residuos al cuadrado de la regresion
residuo2 <- (ejercicio2$residuals)^2
exogeneidad2 <- lm(residuo2 ~ jtpa_offer, data = jtpa_2)
summary(exogeneidad2)

#Ver si alguna variable de la regresion (distinta de jtpa_training) afecta a la VI
exogeneidad3 <- lm(jtpa_offer ~ secund_compl + black + hispanic + married + sex + wkless13, data = jtpa_2)
summary(exogeneidad3)

#Ver si la VI es muy significativa en jtpa_training
relevancia <- lm(jtpa_training ~ jtpa_offer, data = jtpa_2)
summary(relevancia)



#Forma reducida
forma_red <- lm(log_ingreso ~ jtpa_offer + secund_compl + black + hispanic + 
married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_2)
summary(forma_red)
alfa <- forma_red$coefficients["jtpa_offer"]

print(alfa)

#Primera Etapa
primera_etapa <- lm(jtpa_training ~ jtpa_offer + married + black + hispanic + secund_compl + 
                      wkless13 + age2225 + age2629 + age3035 +
                      age3644 + age4554, data = jtpa_2)
summary(primera_etapa)

gamma <- primera_etapa$coefficients["jtpa_offer"]
 print(gamma)

alfa/gamma 

#Coeficiente β1 VI, que reemplazaria al β1 MCO que es sesgado
#Se llega al mismo resultado utilizando

ivreg(log_ingreso ~ jtpa_training + secund_compl + black + hispanic + 
        married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554 | 
        jtpa_offer + secund_compl + black + hispanic + 
        married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_2)
 

#Separando por sexos

jtpa_hombres <- jtpa_2 %>% 
  filter(sex == 1)

jtpa_mujeres <- jtpa_2 %>% 
  filter(sex == 0)

#Punto 1
#Hombres
ejercicio1h <- lm(ingreso ~ jtpa_training, data = jtpa_hombres)
summary(ejercicio1h)

#Modelo Log-Lin
ejercicio1.1h <- lm(log_ingreso ~ jtpa_training, data = jtpa_hombres)
summary(ejercicio1.1h)


#Mujeres
ejercicio1m <- lm(ingreso ~ jtpa_training, data = jtpa_mujeres)
summary(ejercicio1m)

#Modelo Log-Lin
ejercicio1.1m <- lm(log_ingreso ~ jtpa_training, data = jtpa_mujeres)
summary(ejercicio1.1m)

#Punto 2
#Hombres
ejercicio2h <- lm(log_ingreso ~ jtpa_training + sex + jtpa_training*sex + secund_compl + black 
                 + hispanic + married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_hombres)
summary(ejercicio2h)

linearHypothesis(ejercicio2h, "jtpa_training:sex = 0")

#Mujeres
ejercicio2m <- lm(log_ingreso ~ jtpa_training + sex + jtpa_training*sex + secund_compl + black 
                 + hispanic + married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_mujeres)
summary(ejercicio2m)

linearHypothesis(ejercicio2, "jtpa_training:sex = 0")



#Punto 4
#Hombres
#Ver si alguna variable de la regresion (distinta de jtpa_training) afecta a la VI
exogeneidad3h <- lm(jtpa_offer ~ secund_compl + black + hispanic + married + sex + wkless13, data = jtpa_hombres)
summary(exogeneidad3h)

#Ver si la VI es muy significativa en jtpa_training
relevanciah <- lm(jtpa_training ~ jtpa_offer, data = jtpa_hombres)
summary(relevanciah)


#Mujeres
#Ver si alguna variable de la regresion (distinta de jtpa_training) afecta a la VI
exogeneidad3m <- lm(jtpa_offer ~ secund_compl + black + hispanic + married + sex + wkless13, data = jtpa_mujeres)
summary(exogeneidad3m)

#Ver si la VI es muy significativa en jtpa_training
relevanciam <- lm(jtpa_training ~ jtpa_offer, data = jtpa_mujeres)
summary(relevanciam)


#Hombres
ivreg(log_ingreso ~ jtpa_training + secund_compl + black + hispanic + 
        married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554 | 
        jtpa_offer + secund_compl + black + hispanic + 
        married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_hombres)


#Mujeres
ivreg(log_ingreso ~ jtpa_training + secund_compl + black + hispanic + 
        married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554 | 
        jtpa_offer + secund_compl + black + hispanic + 
        married + wkless13 + age2225 + age2629 + age3035 + age3644 + age4554, data = jtpa_mujeres)
