! { dg-do compile }
! { dg-options "-std=f95" }

! PR fortran/29785
! PR fortran/45016
! Check for F2003 rejection of pointer remappings.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, TARGET :: arr(12)
  INTEGER, POINTER :: vec(:), mat(:, :)

  vec => arr ! This is ok.

  vec(2:) => arr ! { dg-error "Fortran 2003" }
  mat(1:2, 1:6) => arr ! { dg-error "Fortran 2003" }
END PROGRAM main
