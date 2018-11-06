### import "computers"

### Estatistica descritiva
# Importa lib
library(pastecs)

# Extrai dados descritivos
desc <- stat.desc(dados)
desc_price <- stat.desc(dados[, 2])

# Formata o dado
desc_price <- format(desc_price, scientific = FALSE, digits = 2, nsmall = 2)

### Gráficos
# Colunas (premium)

# Conta as ocorrencias
cont <- table(dados$premium)
barplot(cont, main = 'Coluna', xlab = 'premium?', ylab = 'frequencia')

# Pizza
pie(cont, main='Pizza', labels=c('não', 'sim'))

# Dispersão, plota x=hd y=price
plot(dados$hd, dados$price, main='Dispersão', xlab='hd', ylab='preço', pch=19)

### Distribuição de frequencia "price"
sturges <- 1+3.3*log10(nrow(dados))
# Variação
dRange <- max(dados$price) - min(dados$price)
# Amplitude (variação/)
dAmp <- dRange/sturges

# Transforma em múltiplos de 5
dAmp5 <- 5*round(dAmp / 5)
xMin5 <- 5*round(min(dados$price) / 5) - 5
xMax5 <- 5*round(max(dados$price) / 5)

# Sequência numerica de min a max+amp com saltos (by) de amp
limites <- seq(xMin5, xMax5 + dAmp5, by = dAmp5)

# Responsavel pro criar as classes e contar quantas ocorrencias dentro de cada classe / Aberto a direita
cortes_aux <- cut(dados$price, limites, right = FALSE, dig.lab = 4)

# Converte para dataframe
price_freq <- as.data.frame(cbind(table(cortes_aux)))
sum(price_freq[, 1])

# Frequencia relativa
for(i in 1:nrow(price_freq)) {
  price_freq[i, 2] <- round(price_freq[i, 1] / sum(price_freq[, 1]), digits = 4)
}

# Renomeia colunas para frequencia e frenquencia relativa
colnames(price_freq) <- c('f', 'f_rel')

### Normalidade
# Histograma => grafico de barra para distribuição de frequencia
hist(dados$price)

# Densidade de probabilidade
hist(dados$price, probability = TRUE)

# Plota distrubuição normal (média e desvio padrão dos dados) no histograma
curve(dnorm(x, mean = mean(dados$price), sd = sd(dados$price)), add = TRUE)

### Analise de regressão linear
# Simples (hd x preço)

# Linha de tendencia
scatter.smooth(x = dados$hd, y = dados$price)

# Correlação dos dados / Confiança na predição
r <- cor(x = dados$hd, y = dados$price)

# Linear Model: Preço previsto pelo tamanho do hd
reg <- lm(dados$price ~ dados$hd)

# Sumário do objeto
summary(reg) # Função para prever o preço: Preço = 0,97.hd [hd => Estimate]  + 1817 [Intercept => Estimate]
# Coeficiente da predição/regressão => Multiple R-squared:  0.1851

# Multipla
reg_mult <- lm(dados$price ~ dados$speed + dados$hd + dados$ram + dados$screen)
summary(reg_mult)
# Coeficiente da predição/regressão => Multiple R-squared:  0.4586

### Predição para primeiro dado
ref <- dados$price[1]

# Predição simples
pred_simples <- predict(reg, dados)[1]

# Predição multipla
pred_mult <- predict(reg_mult, dados)[1]

# mode() <= Retorna o tipo do dado / var

### Classificação
library(caret)
library(randomForest)

# Converte para dataframe
dados <- as.data.frame(dados)

# Indice aleatorio para separar treino e teste / proporção 70%
random_idx <- createDataPartition(dados[, 2], p = 0.7, list = FALSE)

# Cria conjunto treino e teste
dados_treino <- dados[random_idx,]
dados_teste <- dados[-random_idx,]

# factor => transforma dados categoricos [(no, yes), (0, 1)]
dados_treino$multi <- factor(dados_treino$multi)
dados_treino$cd <- factor(dados_treino$cd)
dados_teste$multi <- factor(dados_teste$multi)
dados_teste$cd <- factor(dados_teste$cd)

# Cria modelo RandomForest e preve se é premium com base em todos os outros dados
# ~ . => abreviação para todos dados
rf_model <- randomForest(factor(premium) ~ ., data = dados_treino, controls = cforest_unbiased(ntree =  1000, mtr = 2))
rf_pred <- predict(rf_model, dados_teste)

# Matriz de confusão, cruza dados da previsão do modelo com dados verdadeiros
ref_tab_acuracia <- table(rf_pred, dados_teste$premium)

# Calcula percentual de acuracia
rf_acuracia <- 1 - ((ref_tab_acuracia[2, 1] + ref_tab_acuracia[1, 2]) / nrow(dados_teste))

# Print
paste("Acurácia: ", round(rf_acuracia, 2))

library(gdata)

# Lista objetos na memória
Memory <- ll()
Memory 