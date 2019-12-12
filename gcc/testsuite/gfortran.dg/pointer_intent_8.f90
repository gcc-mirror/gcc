! { dg-do run }
! PR 85797 - this used to get wrong results.


PROGRAM testfortran2
  IMPLICIT NONE

  INTEGER, DIMENSION(10), TARGET :: i4array

  i4array = (/ 1,2,3,4,5,6,7,8,9,10 /)

  call InRef(i4array)

CONTAINS

  subroutine InRef(v)
    INTEGER, DIMENSION(:), POINTER, INTENT(in) :: v
    INTEGER :: i
    if (any (v /= [(i,i=1,10)])) stop 1
  END subroutine

END
