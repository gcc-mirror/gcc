! { dg-do run }
! Structure constructor with component naming.

PROGRAM test
  IMPLICIT NONE

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i
    REAL :: r
    COMPLEX :: c
    LOGICAL :: l
  END TYPE basics_t

  TYPE(basics_t) :: basics

  basics = basics_t (42, -1.5, c=(.5, .5), l=.FALSE.)
  IF (basics%i /= 42 .OR. basics%r /= -1.5 &
      .OR. basics%c /= (.5, .5) .OR. basics%l) THEN
    CALL abort()
  END IF

  basics = basics_t (r=-1.5, i=42, l=.FALSE., c=(.5, .5))
  IF (basics%i /= 42 .OR. basics%r /= -1.5 &
      .OR. basics%c /= (.5, .5) .OR. basics%l) THEN
    CALL abort()
  END IF

END PROGRAM test
