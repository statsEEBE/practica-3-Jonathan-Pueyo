#Solucion Pregunta 1

# ensayo de Bernulli 
x <- c(0, 1)
fx <- c(0.68, 0.32)

cbind(x, fx) # tabla de probabilidad

plot(x, fx, ylim=c(0, 1), type="h", col="red")   # para funciones matematicas - barplot para datos
points(x, fx, pch=16, col="red")

# probabilidades (modelos) = "chinchetas"
# datos = barras 



#modelo aleatorio
n <- 400000
muestra <- sample(x, n, fx, replace = TRUE)
# fi <- table(muestra)   #frecuencia absoluta
fi <- table(muestra)/n    # frecuenia relativa

br <- barplot(fi, ylim=c(0, 1))
lines(br, fx, ylim=c(0, 1), type="h", col="red") 
points(br, fx, pch=16, col="red")



# datos
xbar <- mean(muestra) #promedio

# modelo 
mu <- sum(x*fx) #media 
ssq <- var(muestra) # varianza muestral
sigmasq <- sum((x-mu)^2*fx)# varianza de la funcioon de masa de probabilidad

fx[1]*fx[2] #en bernulli




#resolucion ejercicio
n <- 43
set.seed(123) # ¡¡¡Parcial: deja de ser aleatorio la funcion sample!!!
muestra <- sample (x, n, fx, replace=TRUE)

y <- function(i){sum(sample (x, n, fx, replace=TRUE))}
y(1)

# bucle en R
set.seed(123)
m <- 400000 # encuestas en n=43
encuestas <- sapply(1:m, y)
fi = table(encuestas)/m
data.frame(fi)

dbinom(13, 43, 0.32)

#tabla de probabilidad
resultados <- 1:43
fy <- dbinom(resultados, 43, 0.32)

tabladeprob <- cbind(resultados, fy)
tabladeprob

#b)
resultados <- 1:44
fy <- dbinom(resultados, 44, 0.32)

tabladeprob <- cbind(resultados, fy)
tabladeprob

plot (resultados, fy, type="h", col="red", ylim=c(0,0.2))

Fy <- cumsum(fy)
tabladeprob <- cbind(resultados, fy, Fy)

plot(Fy, type="s", col="red")

pbinom(17, 44, 0.32) # para Q2


# c)
resultados <- 1:24
fy <- dbinom(resultados, 24, 0.68)
Fy <- cumsum(fy)
tabladeprob <- cbind(resultados, fy, Fy)
tabladeprob

mu <- sum(resultados*fy) #media o valor esperado 
mu

sigmasq <- sum((resultados-mu)^2*fy) #varianza
sigmasq

# primer cuartil => en la tabla de probabilidad nos fijamos en la 
# frecuencia acumulada (Fy) y escogemos el valor inmediatamente superior a 0.25
# q(0.25) = 15 en este caso

qbinom(0.25, 24, 0.68)


# d)






