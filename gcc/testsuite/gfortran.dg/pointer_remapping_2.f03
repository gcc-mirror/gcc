! { dg-do compile }
! { dg-options "-std=f2003" }

! PR fortran/29785
! Check for F2008 rejection of rank remapping to rank-two base array.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE
  INTEGER, TARGET :: arr(12), basem(3, 4)
  INTEGER, POINTER :: vec(:), mat(:, :)

  ! These are ok.
  vec => arr
  vec(2:) => arr
  mat(1:2, 1:6) => arr

  vec(1:12) => basem ! { dg-error "Fortran 2008" }
END PROGRAM main
