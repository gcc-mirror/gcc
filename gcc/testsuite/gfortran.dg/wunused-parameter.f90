! { dg-do compile }
! { dg-options "-Wunused-parameter" }
! PR66605
MODULE test
  IMPLICIT NONE
  INTEGER, PARAMETER :: wp = KIND(1.0D0)
CONTAINS
SUBROUTINE sub (neq, time, y, dydt)
  IMPLICIT NONE    
    INTEGER :: neq
    REAL(WP) :: time, y(neq), dydt(neq)

    dydt(1) = 1.0 / y(1)
END SUBROUTINE sub
END MODULE
