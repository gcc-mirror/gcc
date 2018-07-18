! { dg-do run }
! Check the fix for PR33568 in which the optional KIND
! argument for ANINT, with an array for the first argument
! would cause an ICE.
!
! Contributed by Ignacio Fernández Galván <jellby@yahoo.com>
!
PROGRAM Test
  IMPLICIT NONE
  INTEGER, PARAMETER :: DP=8
  REAL(DP), DIMENSION(1:3) :: A = (/1.76,2.32,7.66/), B
  A = ANINT ( A , DP)
  B = A
  A = ANINT ( A)
  if (any (A .ne. B)) STOP 1
END PROGRAM Test
