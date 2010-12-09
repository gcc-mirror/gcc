! { dg-do compile }
! PR fortran/46794

! Check that results of powers of integers with kinds 1 and 2 are
! correctly converted back; this used to ICE because a conversion
! from kind 4 to the correct one was missing.

! Contributed by Daniel Kraft, d@domob.eu.

PROGRAM main
  IMPLICIT NONE

  INTEGER(KIND=1) :: k1
  INTEGER(KIND=2) :: k2

  k1 = 1_1
  k2 = 1_2

  k1 = 1_1 + 1_1**k1
  k2 = 1_2 + 1_2**k2

  k2 = 1_1 + 1_1**k2
  k2 = 1_1 + 1_2**k1
  k2 = 1_1 + 1_2**k2
END PROGRAM main
