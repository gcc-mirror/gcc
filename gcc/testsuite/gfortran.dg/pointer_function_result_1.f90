! { dg-do run }
! Test the fix for PR47844, in which the stride in the function result
! was ignored. Previously, the result was [1,3] at lines 15 and 16.
!
! Contributed by KePu  <Kdx1999@gmail.com>
!
PROGRAM test_pointer_value
  IMPLICIT NONE
  INTEGER, DIMENSION(10), TARGET :: array= [1,3,5,7,9,11,13,15,17,19]
  INTEGER, dimension(2) :: array_fifth
  INTEGER, POINTER, DIMENSION(:) :: ptr_array => NULL()
  INTEGER, POINTER, DIMENSION(:) :: ptr_array_fifth => NULL()
  ptr_array => array
  array_fifth = every_fifth (ptr_array)
  if (any (array_fifth .ne. [1,11])) STOP 1
  if (any (every_fifth(ptr_array) .ne. [1,11])) STOP 2
CONTAINS
  FUNCTION every_fifth (ptr_array) RESULT (ptr_fifth)
    IMPLICIT NONE
    INTEGER, POINTER, DIMENSION(:) :: ptr_fifth
    INTEGER, POINTER, DIMENSION(:), INTENT(in) :: ptr_array
    INTEGER :: low
    INTEGER :: high
    low = LBOUND (ptr_array, 1)
    high = UBOUND (ptr_array, 1)
    ptr_fifth => ptr_array (low: high: 5) 
  END FUNCTION every_fifth
END PROGRAM test_pointer_value
