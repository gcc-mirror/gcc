! { dg-do compile }
! { dg-options "-std=f95" }

! BLOCK should be rejected without F2008.

PROGRAM main
  IMPLICIT NONE

  BLOCK ! { dg-error "Fortran 2008" }
    INTEGER :: i
  END BLOCK
END PROGRAM main
