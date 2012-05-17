! { dg-do compile }
!
! PR 41070: [4.5 Regression] Error: Components of structure constructor '' at (1) are PRIVATE
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>

MODULE cdf_aux_mod
IMPLICIT NONE

TYPE :: one_parameter
  CHARACTER (8) :: name
END TYPE one_parameter

TYPE :: the_distribution
  CHARACTER (8) :: name
END TYPE the_distribution

TYPE (the_distribution), PARAMETER :: the_beta = the_distribution('cdf_beta')
END MODULE cdf_aux_mod

SUBROUTINE cdf_beta()
  USE cdf_aux_mod
  IMPLICIT NONE
  CALL check_complements(the_beta%name)
END SUBROUTINE cdf_beta
