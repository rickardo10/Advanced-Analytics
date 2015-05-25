#install.packages("xlsx")
library("xlsx")

data = read.xlsx(file = "vinos.xlsx", sheetIndex = 1)
data = subset(x = data, select = -NA.)

mean = sapply(X = data, FUN = mean)

stem(data$Magnesium)
hist(x = data$Magnesium, col = c("red", "blue", "yellow"), ylim = c(0,60) )

## Deteccion de outliers
# Metodo gráfico
norm = scale(x = data, center = TRUE, scale = TRUE)
norm = as.data.frame(norm)
plot(norm$Magnesium, pch = 16, col = "red")
abline(h = 3.509, col = "blue")


# Prueba de Hampel
r = abs( data$Magnesium - median(data$Magnesium ) )
MAD = median( r )

sup3 = which(x = r >= 3 * MAD )
sup4 = which(x = r >= 4.5 * MAD )

# Caja y bigote
a = boxplot(data$Magnesium, notch=TRUE, horizontal = TRUE, col= "Red")
sup5 = which( x = data$Magnesium %in% a$out )

# Método de quality control charts
#install.packages("qcc")
library("qcc")

control = qcc( data = data$Magnesium, type = "xbar.one")
sup6 = control$violations$beyond.limits


## Verificar cambio de variables en las clases
boxplot( data$Magnesium ~ factor( data$Tipo ), horizontal = TRUE, col = "Red" )

## Pruebas normalidad
#install.packages("e1071")
library("e1071")

# Variable 1

par( mfrow = c(2,2) )
qqnorm( data$Magnesium, col = "Red", pch = 15 )
qqline(data$Magnesium, col = "Blue")

P = probplot( data$Magnesium )
lines( P, v=median(data$Magnesium ), col = "Blue" )

hist( data$Magnesium, col = "Gold", breaks = "Scott")

boxplot( data$Magnesium, col = "Pink")

#install.packages("normtest")
library("normtest")
normtest::jb.norm.test(x = data$Magnesium )

# Variable 2

par( mfrow = c(2,2) )
qqnorm( data$Ash, col = "Red", pch = 15 )
qqline(data$Ash, col = "Blue")

P = probplot( data$Ash )
lines( P, v=median(data$Ash ), col = "Blue" )

hist( data$Ash, col = "Gold", breaks = "Scott")

which( data$Ash %in% boxplot( data$Ash, col = "Pink")$out )

normtest::jb.norm.test(x = data$Ash )
normtest::wb.norm.test(x = data$Ash )


## Outliers multivariable
# Método Mahalanobis
# Dos variables
multi = mqcc( list( norm$Ash, norm$Magnesium ), confidence.level = 0.99 )

#install.packages("ellipse")
library("ellipse")
puntos = ellipse( multi$cov, centre = multi$center, level = 0.99, npoints = 300 )
plot( norm$Ash, norm$Magnesium, pch = 15, col = "orange3" )
points(puntos, pch= 12, col = "Red")

# Todas las variables
multi = mqcc( norm, confidence.level = 0.99 )
sup7 = multi$violations$beyond.limits

# Local Outlier Factor
#install.packages("DMwR")
library("DMwR")

# K = 5
valorLOF = lofactor( data = data.frame( norm$Ash, norm$Magnesium ), k = 5 )
plot( valorLOF, pch = 16, col="Red" )

plot( density(valorLOF) )

anomalos = order( valorLOF, decreasing = TRUE )[1:6]
data.frame( data$Ash, data$Magnesium )[anomalos,]

# K = 10
valorLOF = lofactor( data = data.frame( norm$Ash, norm$Magnesium ), k = 10 )
plot( valorLOF, pch = 16, col="Red" )

plot( density(valorLOF) )

anomalos = order( valorLOF, decreasing = TRUE )[1:6]
data.frame( data$Ash, data$Magnesium )[anomalos,]


# K = 10 y todas las varioables
valorLOF = lofactor( data = norm, k = 10 )
plot( valorLOF, pch = 16, col="Red" )

plot( density(valorLOF) )

anomalos = order( valorLOF, decreasing = TRUE )[1:8]
data[anomalos,]

#install.packages("mvoutlier")
library(mvoutlier)
aq.plot( norm, delta = qchisq(0.99, df = ncol(norm) ) )
