! { dg-do "compile" }

! Abstract Types.
! Check for parser errors.

MODULE m
  IMPLICIT NONE

  TYPE, ABSTRACT, EXTENDS(abst_t), ABSTRACT :: error_t ! { dg-error "Duplicate ABSTRACT attribute" }
    INTEGER :: y
  END TYPE error_t ! { dg-error "END MODULE" }

END MODULE m
