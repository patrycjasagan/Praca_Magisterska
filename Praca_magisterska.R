library(readxl)
library(psych)
library(ggplot2)
library(farver)
library(aTSA)
library(vars)
library(corrgram)
library(lmtest)
library(corrplot)
library(nortest)
library(tseries)
library(NlinTS)


# Importowanie danych
data <- read_excel("dane_miesieczne.xlsx")
data <- data.frame(lapply(data, as.numeric))
summary(data)
head(data)


#Wykresy czasowe
data_date <- data
data_date$Date <- seq(as.Date("2010-01-01"), by = "month", length.out = nrow(data_date))


ggplot(data_date, aes(x = Date, y = PROD)) +
  geom_line() +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 24))


ggplot(data_date, aes(x = Date, y = UnempRate)) +
  geom_line() +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 24))


ggplot(data_date, aes(x = Date, y = IntRate)) +
  geom_line() +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 24))


ggplot(data_date, aes(x = Date, y = CPI)) +
  geom_line() +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 24))


ggplot(data_date, aes(x = Date, y = Import)) +
  geom_line() +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 24))


ggplot(data_date, aes(x = Date, y = Export)) +
  geom_line() +
  theme_minimal() +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, size = 1),
    text = element_text(size = 24))


#Statystyki opisowe
round(describe(data),2)

#Wspolczynnik zmiennosci
wsp_zm <- c()
for (i in 1:ncol(data)){
  wsp_zm[i] <- sd(data[,i]) / mean(data[,i])
}
wsp_zm <- data.frame(t(wsp_zm))
colnames(wsp_zm) <- c("PROD","UnempRate","IntRate","CPI","Import","Export")
round(wsp_zm,2)


#Macierz korelacji
cor_matrix <- cor(data)
print(cor_matrix)
corrplot(cor_matrix, method = "circle")

#Wartosc krytyczna korelacji
t_critical <- qt(1-0.05/2, df = 166)
r_critical <- t_critical/(sqrt(t_critical^2 +166))
r_critical


#Badanie stacjonarnosci
#Test ADF
adf_PROD <- aTSA::adf.test(data$PROD, nlag=10)
adf_UnempRate <- aTSA::adf.test(data$UnempRate, nlag=10)
adf_IntRate <- aTSA::adf.test(data$IntRate, nlag=10)
adf_CPI <- aTSA::adf.test(data$CPI, nlag=10)
adf_Import <- aTSA::adf.test(data$Import, nlag=10)
adf_Export <- aTSA::adf.test(data$Export, nlag=10)

adft1 <- data.frame(adf_PROD$type1[,3],adf_UnempRate$type1[,3],adf_IntRate$type1[,3],adf_CPI$type1[,3],
                    adf_Import$type1[,3],adf_Export$type1[,3])
adft2 <- data.frame(adf_PROD$type2[,3],adf_UnempRate$type2[,3],adf_IntRate$type2[,3],adf_CPI$type2[,3],
                    adf_Import$type2[,3],adf_Export$type2[,3])
adft3 <- data.frame(adf_PROD$type3[,3],adf_UnempRate$type3[,3],adf_IntRate$type3[,3],adf_CPI$type3[,3],
                    adf_Import$type3[,3],adf_Export$type3[,3])
colnames(adft1) <- colnames(wsp_zm)
colnames(adft2) <- colnames(adft3) <- colnames(adft1) 
adft1
adft2
adft3


optimal_lag <- VARselect(data$PROD, lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(data$UnempRate, lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(data$IntRate, lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(data$CPI, lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(data$Import, lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(data$Export, lag.max = 10)
optimal_lag$selection

#Test KPSS
kpss_PROD <- aTSA::kpss.test(data$PROD)
kpss_UnempRate <- aTSA::kpss.test(data$UnempRate)
kpss_IntRate <- aTSA::kpss.test(data$IntRate)
kpss_CPI <- aTSA::kpss.test(data$CPI)
kpss_Import <- aTSA::kpss.test(data$Import)
kpss_Export <- aTSA::kpss.test(data$Export)



#Roznicowanie zmiennych
data <- data.frame(lapply(data, diff))

#Rzad opoznienia
nr_lags <- VARselect(data,lag.max=10)
nr_lags


#Model VAR
model <- VAR(data,p=10)
summary(model)
residuals <- resid(model)


#Badanie autokorelacji reszt modelu VAR
korelogram_PROD <- acf(residuals[,1], lag.max=NULL)
korelogram_UnempRate <- acf(residuals[,2], lag.max=NULL)
korelogram_IntRate <- acf(residuals[,3], lag.max=NULL)
korelogram_CPI <- acf(residuals[,4], lag.max=NULL)
korelogram_Import <- acf(residuals[,5], lag.max=NULL)
korelogram_Export <- acf(residuals[,6], lag.max=NULL)

plot(korelogram_PROD[1:10],ci=0.95, main="Korelogram dla reszt równania I - (PROD)")
plot(korelogram_UnempRate[1:10],ci=0.95, main="Korelogram dla reszt równania II - (UnempRate)")
plot(korelogram_IntRate[1:10],ci=0.95, main="Korelogram dla reszt równania III - (IntRate)")
plot(korelogram_CPI[1:10],ci=0.95, main="Korelogram dla reszt równania IV - (CPI)")
plot(korelogram_Import[1:10],ci=0.95, main="Korelogram dla reszt równania V - (Import)")
plot(korelogram_Export[1:10],ci=0.95, main="Korelogram dla reszt równania VI - (Export)")


#Badanie zgodnosci rozkladu reszt modelu VAR z rozkladem normalnym
jb_test <- c()
shapiro_test <- c()
lillie_test <- c()

for (i in 1:6){
  jb_test[i] <- (jarque.bera.test(residuals[,i]))$p.value
  shapiro_test[i] <- (shapiro.test(residuals[,i]))$p.value
  lillie_test[i] <- (lillie.test(residuals[,i]))$p.value
}

normality_tests <- data.frame(
  Shapiro_Wilk_p_value = shapiro_test,
  Jarque_Bera_P_value = jb_test,
  Lilliefors_p_value = lillie_test
)

print(normality_tests)


#Badanie stacjonarnosci reszt nodelu VAR
#Test ADF
adf_PROD_resid <- aTSA::adf.test(residuals[,1], nlag=10)
adf_UnempRate_resid <- aTSA::adf.test(residuals[,2], nlag=10)
adf_IntRate_resid <- aTSA::adf.test(residuals[,3], nlag=10)
adf_CPI_resid <- aTSA::adf.test(residuals[,4], nlag=10)
adf_Import_resid <- aTSA::adf.test(residuals[,5], nlag=10)
adf_Export_resid <- aTSA::adf.test(residuals[,6], nlag=10)

adft1_resid <- data.frame(adf_PROD_resid$type1[,3],adf_UnempRate_resid$type1[,3],adf_IntRate_resid$type1[,3],
                    adf_CPI_resid$type1[,3],adf_Import_resid$type1[,3],adf_Export_resid$type1[,3])
adft2_resid <- data.frame(adf_PROD_resid$type2[,3],adf_UnempRate_resid$type2[,3],adf_IntRate_resid$type2[,3],
                    adf_CPI_resid$type2[,3],adf_Import_resid$type2[,3],adf_Export_resid$type2[,3])
adft3_resid <- data.frame(adf_PROD_resid$type3[,3],adf_UnempRate_resid$type3[,3],adf_IntRate_resid$type3[,3],
                    adf_CPI_resid$type3[,3],
                    adf_Import_resid$type3[,3],adf_Export_resid$type3[,3])
colnames(adft1_resid) <- colnames(wsp_zm)
colnames(adft2_resid) <- colnames(adft3_resid) <- colnames(adft1) 
adft1_resid
adft2_resid
adft3_resid

optimal_lag <- VARselect(residuals[,1], lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(residuals[,2], lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(residuals[,3], lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(residuals[,4], lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(residuals[,5], lag.max = 10)
optimal_lag$selection
optimal_lag <- VARselect(residuals[,6], lag.max = 10)
optimal_lag$selection

#Test KPSS
kpss_PROD <- aTSA::kpss.test(residuals[,1])
kpss_UnempRate <- aTSA::kpss.test(residuals[,2])
kpss_IntRate <- aTSA::kpss.test(residuals[,3])
kpss_CPI <- aTSA::kpss.test(residuals[,4])
kpss_Import <- aTSA::kpss.test(residuals[,5])
kpss_Export <- aTSA::kpss.test(residuals[,6])


#Test przyczynowości Grangera 
granger_results <- matrix(NA, ncol = ncol(data), nrow = ncol(data))
colnames(granger_results) <- rownames(granger_results) <- colnames(wsp_zm)

for (i in 1:ncol(data)) {
  for (j in 1:ncol(data)) {
    if (i != j) {
      var_model_ij <- VAR(data[, c(i, j)], p = 10)
      test_result <- causality(var_model_ij, cause = colnames(data)[i])
      granger_results[i, j] <- test_result$Granger$p.value
    } else {
      granger_results[i, j] <- NA
    }
  }
}
round(granger_results,8)


#Analiza reakcji na impuls
#PROD -> Export
model_PROD_Export <- VAR(data[, c("PROD", "Export")], p = 10)
irf_PROD_Export <- irf(model_PROD_Export, impulse = "PROD", response = "Export", n.ahead = 10, boot = FALSE)
plot(irf_PROD_Export, main = "", ylab="")

#UnempRate -> Export
model_UnempRate_Export <- VAR(data[, c("UnempRate", "Export")], p = 10)
irf_UnempRate_Export <- irf(model_UnempRate_Export, impulse = "UnempRate", response = "Export", n.ahead = 10, boot = FALSE)
plot(irf_UnempRate_Export, main = "", ylab="")

#CPI -> Export
model_CPI_Export <- VAR(data[, c("CPI", "Export")], p = 10)
irf_CPI_Export <- irf(model_CPI_Export, impulse = "CPI", response = "Export", n.ahead = 10, boot = FALSE)
plot(irf_CPI_Export, main = "", ylab="")

#Import -> Export
model_Import_Export <- VAR(data[, c("Import", "Export")], p = 10)
irf_Import_Export <- irf(model_Import_Export, impulse = "Import", response = "Export", n.ahead = 10, boot = FALSE)
plot(irf_Import_Export, main = "", ylab="")

#####################################################################################
#NlinTS

data <- read_excel("dane_miesieczne.xlsx")
data <- as.data.frame(lapply(data, as.numeric))
data <- data.frame(lapply(data, diff))

# Optymalizacja parametrow LayersUniv i LayersBiv
LayersUniv <- c(1, 2, 3, 4)
LayersBiv <- c(1, 2, 3, 4)

lag <- 10
iters <- 50
learningRate <- 0.01
algo <- "sgd"
batch_size <- 16
bias <- TRUE
seed <- 1234

#UnempRate -> PROD
effect <- data[,1]
cause <- data[,2]

#PROD -> UnempRate
effect <- data[,2]
cause <- data[,1]

#IntRate -> PROD
effect <- data[,1]
cause <- data[,3]

#PROD -> IntRate
effect <- data[,3]
cause <- data[,1]

#CPI -> PROD
effect <- data[,1]
cause <- data[,4]

#PROD -> CPI
effect <- data[,4]
cause <- data[,1]

#Import -> PROD
effect <- data[,1]
cause <- data[,5]

#PROD -> Import
effect <- data[,5]
cause <- data[,1]

#Export -> PROD
effect <- data[,1]
cause <- data[,6]

#PROD -> Export
effect <- data[,6]
cause <- data[,1]

#IntRate -> UnempRate
effect <- data[,2]
cause <- data[,3]

#UnempRate -> IntRate
effect <- data[,3]
cause <- data[,2]

#CPI -> UnempRate
effect <- data[,2]
cause <- data[,4]

#UnempRate -> CPI
effect <- data[,4]
cause <- data[,2]

#Import -> UnempRate
effect <- data[,2]
cause <- data[,5]

#UnempRate -> Import
effect <- data[,5]
cause <- data[,2]

#Export -> UnempRate
effect <- data[,2]
cause <- data[,6]

#UnempRate -> Export
effect <- data[,6]
cause <- data[,2]

#CPI -> IntRate
effect <- data[,3]
cause <- data[,4]

#IntRate -> CPI
effect <- data[,4]
cause <- data[,3]

#Import -> IntRate
effect <- data[,3]
cause <- data[,5]

#IntRate -> Import
effect <- data[,5]
cause <- data[,3]

#Export -> IntRate
effect <- data[,3]
cause <- data[,6]

#IntRate -> Export
effect <- data[,6]
cause <- data[,3]

#Import -> CPI
effect <- data[,4]
cause <- data[,5]

#CPI -> Import
effect <- data[,5]
cause <- data[,4]

#Export -> CPI
effect <- data[,4]
cause <- data[,6]

#CPI -> Export
effect <- data[,6]
cause <- data[,4]

#Export -> Import
effect <- data[,5]
cause <- data[,6]

#Import -> Export
effect <- data[,6]
cause <- data[,5]


results <- data.frame(matrix(NA, nrow=16, ncol=4))
colnames(results) <- c("LayersUniv", "LayersBiv", "GCI", "p_value")

# Testowanie wszystkich kombinacji LayersUniv i LayersBiv
row_index <- 1
for (univ in LayersUniv) {
  for (biv in LayersBiv) {
    model <- nlin_causality.test(
      effect, 
      cause, 
      lag,
      LayersUniv=c(univ), 
      LayersBiv=c(biv), 
      iters=iters, 
      learningRate, 
      algo, 
      batch_size, 
      bias,
      seed
    )
    
    results[row_index, ] <- c(univ, biv, model$gci, model$pvalue)
    row_index <- row_index + 1
  }
}

print(round(results,9))


# Optymalizacja parametru iters
iters <- c(50,100,200)

lag <- 10
learningRate <- 0.01
algo <- "sgd"
batch_size <- 16
bias <- TRUE
seed <- 1234

results <- data.frame(matrix(NA, nrow=length(iters), ncol=3))
colnames(results) <- c("Iters", "GCI", "p_value")

row_index <- 1
for (iter in iters) {
  model <- nlin_causality.test(
    effect, 
    cause, 
    lag,
    LayersUniv = c(1), 
    LayersBiv= c(1), 
    iters=iter, 
    learningRate, 
    algo, 
    batch_size, 
    bias,
    seed
  )
  
  results[row_index, ] <- c(iter, model$gci, model$pvalue)
  row_index <- row_index + 1
}

print(round(results,9))


# Optymalizacja parametru learningrate
learningRate <- c(0.01, 0.005, 0.001)
lag <- 10
iters <- 50
algo <- "sgd"
batch_size <- 16
bias <- TRUE
seed <- 1234

results <- data.frame(matrix(NA, nrow=length(learningRate), ncol=3))
colnames(results) <- c("LearningRate", "GCI", "p_value")

row_index <- 1
for (learningRate in learningRate) {
  model <- nlin_causality.test(
    effect, 
    cause, 
    lag,
    LayersUniv = c(1), 
    LayersBiv= c(1), 
    iters, 
    learningRate = learningRate, 
    algo, 
    batch_size, 
    bias,
    seed
  )
  
  results[row_index, ] <- c(learningRate, model$gci, model$pvalue)
  row_index <- row_index + 1
}

print(round(results,9))


# Optymalizacja parametru algo
algo <- c("sgd", "adam")

results <- data.frame(matrix(NA, nrow=length(algo), ncol=3))
colnames(results) <- c("Algo", "GCI", "p_value")

row_index <- 1
for (algo in algo) {
  model <- nlin_causality.test(
    effect, 
    cause, 
    lag = 10,
    LayersUniv = c(1), 
    LayersBiv= c(1), 
    iters = 50, 
    learningRate = 0.001, 
    algo = algo, 
    batch_size = 16, 
    bias = TRUE,
    seed = 1234
  )
  
  results[row_index, ] <- c(algo, round(model$gci,7), round(model$pvalue,7))
  row_index <- row_index + 1
}

print(results,9)


# Optymalizacja parametru batch_size
batch_size <- c(16, 32, 64)

results <- data.frame(matrix(NA, nrow=length(batch_size), ncol=3))
colnames(results) <- c("Algo", "GCI", "p_value")

row_index <- 1
for (batch_size in batch_size) {
  model <- nlin_causality.test(
    effect, 
    cause, 
    lag = 10,
    LayersUniv = c(1), 
    LayersBiv= c(1), 
    iters = 50, 
    learningRate = 0.001, 
    algo = "adam", 
    batch_size = batch_size, 
    bias = TRUE,
    seed = 1234
  )
  
  results[row_index, ] <- c(batch_size, round(model$gci,7), round(model$pvalue,7))
  row_index <- row_index + 1
}

print(results,9)


#Finalne wyniki

#UnempRate -> PROD
model = nlin_causality.test (data[,1], data[,2], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#PROD -> UnempRate
model = nlin_causality.test (data[,2], data[,1], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#IntRate -> PROD
model = nlin_causality.test (data[,1], data[,3], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#PROD -> IntRate
model = nlin_causality.test (data[,3], data[,1], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#CPI -> PROD
model = nlin_causality.test (data[,1], data[,4], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#PROD -> CPI
model = nlin_causality.test (data[,4], data[,1], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#Import -> PROD
model = nlin_causality.test (data[,1], data[,5], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#PROD -> Import
model = nlin_causality.test (data[,5], data[,1], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> PROD
model = nlin_causality.test (data[,1], data[,6], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#PROD -> Export
model = nlin_causality.test (data[,6], data[,1], 10, c(1), c(1), 50, 0.001, "adam", 64, TRUE, 1234) 
model$summary ()

#IntRate -> UnempRate
model = nlin_causality.test (data[,2], data[,3], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#UnempRate -> IntRate
model = nlin_causality.test (data[,3], data[,2], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#CPI -> UnempRate
model = nlin_causality.test (data[,2], data[,4], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#UnempRate -> CPI
model = nlin_causality.test (data[,4], data[,2], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#Import -> UnempRate
model = nlin_causality.test (data[,2], data[,5], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#UnempRate -> Import
model = nlin_causality.test (data[,5], data[,2], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> UnempRate
model = nlin_causality.test (data[,2], data[,6], 10, c(3), c(2), 50, 0.01, "sgd", 32, TRUE, 1234) 
model$summary ()

#UnempRate -> Export
model = nlin_causality.test (data[,6], data[,2], 10, c(1), c(1), 50, 0.001, "adam", 64, TRUE, 1234) 
model$summary ()

#CPI -> IntRate
model = nlin_causality.test (data[,3], data[,4], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#IntRate -> CPI
model = nlin_causality.test (data[,4], data[,3], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#Import -> IntRate
model = nlin_causality.test (data[,3], data[,5], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#IntRate -> Import
model = nlin_causality.test (data[,5], data[,3], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> IntRate
model = nlin_causality.test (data[,3], data[,6], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#IntRate -> Export
model = nlin_causality.test (data[,6], data[,3], 10, c(1), c(1), 50, 0.001, "adam", 64, TRUE, 1234) 
model$summary ()

#Import -> CPI
model = nlin_causality.test (data[,4], data[,5], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#CPI -> Import
model = nlin_causality.test (data[,5], data[,4], 10, c(3), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> CPI
model = nlin_causality.test (data[,4], data[,6], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#CPI -> Export
effect <- data[,6]
cause <- data[,4]
model = nlin_causality.test (data[,6], data[,4], 10, c(1), c(1), 50, 0.001, "adam", 64, TRUE, 1234) 
model$summary ()

#Export -> Import
model = nlin_causality.test (data[,5], data[,6], 10, c(3), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#Import -> Export
model = nlin_causality.test (data[,6], data[,5], 10, c(1), c(1), 50, 0.001, "adam", 64, TRUE, 1234) 
model$summary ()


#Alternatywne kombinacje liczby neuronów z istotnymi wynikami p-value

#PROD -> UnempRate
model = nlin_causality.test (data[,2], data[,1], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#PROD -> IntRate
model = nlin_causality.test (data[,3], data[,1], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,1], 10, c(2), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,1], 10, c(4), c(3), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#CPI -> PROD
model = nlin_causality.test (data[,1], data[,4], 10, c(4), c(3), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> PROD
model = nlin_causality.test (data[,1], data[,6], 10, c(4), c(3), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#UnempRate -> IntRate
model = nlin_causality.test (data[,3], data[,2], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,2], 10, c(4), c(3), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#CPI -> UnempRate
model = nlin_causality.test (data[,2], data[,4], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Import -> UnempRate
model = nlin_causality.test (data[,2], data[,5], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> UnempRate
model = nlin_causality.test (data[,2], data[,6], 10, c(4), c(3), 50, 0.01, "sgd", 32, TRUE, 1234) 
model$summary ()

#CPI -> IntRate
model = nlin_causality.test (data[,3], data[,4], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,4], 10, c(2), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,4], 10, c(4), c(3), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Import -> IntRate
model = nlin_causality.test (data[,3], data[,5], 10, c(1), c(1), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,5], 10, c(2), c(2), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,5], 10, c(4), c(3), 50, 0.01, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> IntRate
model = nlin_causality.test (data[,3], data[,6], 10, c(2), c(2), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()
model = nlin_causality.test (data[,3], data[,6], 10, c(4), c(3), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()

#Export -> Import
model = nlin_causality.test (data[,5], data[,6], 10, c(4), c(3), 50, 0.005, "sgd", 16, TRUE, 1234) 
model$summary ()




############################################################################################################################

data <- read_excel("dane_miesieczne.xlsx")
data <- as.data.frame(lapply(data, as.numeric))
data <- data.frame(lapply(data, diff))

#Analiza zaleznoci za pomocą transferu entropii dla k=3
te_results <- matrix(NA, ncol = ncol(data), nrow = ncol(data))

x <- colnames(data)
for (i in 1:length(x)) {
  for (j in 1:length(x)) {
    if (i != j) {
      te_value <- te_cont(data[[i]], data[[j]], k = 3, normalize = TRUE)
      te_results[i,j] <- round(te_value,6)
    } else {
      te_results[i,j] <- NA
    }
  }
}

print(t(te_results))


#Analiza zaleznoci za pomocą transferu entropii dla k=5
te_results <- matrix(NA, ncol = ncol(data), nrow = ncol(data))

x <- colnames(data)
for (i in 1:length(x)) {
  for (j in 1:length(x)) {
    if (i != j) {
      te_value <- te_cont(data[[i]], data[[j]], k = 5, normalize = TRUE)
      te_results[i,j] <- round(te_value,6)
    } else {
      te_results[i,j] <- NA
    }
  }
}

print(t(te_results))


#Analiza zaleznoci za pomocą transferu entropii dla k=10
te_results <- matrix(NA, ncol = ncol(data), nrow = ncol(data))

x <- colnames(data)
for (i in 1:length(x)) {
  for (j in 1:length(x)) {
    if (i != j) {
      te_value <- te_cont(data[[i]], data[[j]], k = 10, normalize = TRUE)
      te_results[i,j] <- round(te_value,6)
    } else {
      te_results[i,j] <- NA
    }
  }
}

print(t(te_results))



