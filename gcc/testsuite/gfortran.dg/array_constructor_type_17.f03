! { dg-do compile }
! { dg-options "-fno-range-check -Wconversion" }
! PR fortran/27997
!
! Range check on array-constructors with typespec.

PROGRAM test
  IMPLICIT NONE

  INTEGER(KIND=4) :: arr(1)
  arr = (/ INTEGER(KIND=4) :: HUGE(0_8) /) ! { dg-warning "Conversion" }
END PROGRAM test
