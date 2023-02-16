
#' Getter de la decennie d'une année
#'
#' @param annee 
decennie_a_partir_annee <- function(annee) {
  return(annee - annee %% 10)
}

#' Fonction de calcul de statistique descriptive
#'
#' @inheritParams stats::var
#' @param stat type de statistique a calculer sous forme de chaîne caractères
#' les valeurs autorisées sont moyenne, ecart-type, sd, variance
#'
#' @return
#' @examples
#' calcul_stat_desc(rnorm(10))
#' calcul_stat_desc(rnorm(10), "ecart-type")
#' calcul_stat_desc(rnorm(10), "variance")
calcul_stat_desc <- function(x, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    resultat <- mean(x, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    resultat <- sd(x, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    resultat <- var(x, na.rm = TRUE, ...)
  }
  return(resultat)
}

