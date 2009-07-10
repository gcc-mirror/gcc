! { dg-do compile }
! PR35478 internal compiler error: Segmentation fault
MODULE auxiliary
  IMPLICIT NONE
  INTEGER, PARAMETER, PRIVATE :: dp = SELECTED_REAL_KIND(15)
  INTERFACE median
     MODULE PROCEDURE R_valmed, I_valmed, D_valmed
  END INTERFACE
  PUBLIC  :: median
  PRIVATE :: R_valmed, I_valmed, D_valmed
CONTAINS
  RECURSIVE FUNCTION D_valmed (XDONT) RESULT (res_med)
    Real (kind=dp), Dimension (:), Intent (In) :: XDONT
    Real (kind=dp) :: res_med
    res_med = 0.0d0
  END FUNCTION D_valmed
  RECURSIVE FUNCTION R_valmed (XDONT) RESULT (res_med)
    Real, Dimension (:), Intent (In) :: XDONT
    Real :: res_med
    res_med = 0.0
  END FUNCTION R_valmed
  RECURSIVE FUNCTION I_valmed (XDONT) RESULT (res_med)
    Integer, Dimension (:), Intent (In)  :: XDONT
    Integer :: res_med
    res_med = 0
  END FUNCTION I_valmed
END MODULE auxiliary
PROGRAM main
  USE auxiliary
  IMPLICIT NONE
  INTEGER, PARAMETER    :: dp = SELECTED_REAL_KIND(15)
  REAL(kind=dp) :: rawData(2), data, work(3)
  data = median(rawData, work) ! { dg-error "no specific function" }
END PROGRAM main
! { dg-final { cleanup-modules "auxiliary" } }
