! { dg-do compile }
! { dg-options "-std=f95" }

! Test for rejection of implied-shape prior to Fortran 2008.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, PARAMETER :: arr(*) = (/ 2, 3, 4 /) ! { dg-error "Fortran 2008" }
END PROGRAM main
