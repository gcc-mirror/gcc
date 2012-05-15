! { dg-do compile }
! PR20900 - USE associated variables cannot be equivalenced.
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
MODULE TEST
 INTEGER :: I
END MODULE
! note 11.7
USE TEST, ONLY : K=>I
INTEGER :: L
EQUIVALENCE(K,L) ! { dg-error "conflicts with USE ASSOCIATED attribute" }
END
