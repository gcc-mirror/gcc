! { dg-do compile }
!
! PR fortran/49023
!
PROGRAM test

  USE, INTRINSIC :: iso_c_binding
  IMPLICIT NONE

  TYPE (C_PTR) :: x, y

  PRINT *, C_ASSOCIATED([x,y])  ! { dg-error "'C_PTR_1' argument of 'c_associated' intrinsic at .1. must be a scalar" }

END PROGRAM test
