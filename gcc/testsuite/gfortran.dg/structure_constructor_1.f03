! { dg-do run }
! Simple structure constructors, without naming arguments, default values
! or inheritance and the like.

PROGRAM test
  IMPLICIT NONE

  ! Empty structuer
  TYPE :: empty_t
  END TYPE empty_t

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i
    REAL :: r
    COMPLEX :: c
    LOGICAL :: l
  END TYPE basics_t

  ! Structure with strings
  TYPE :: strings_t
    CHARACTER(len=5) :: str1, str2
    CHARACTER(len=10) :: long
  END TYPE strings_t

  ! Structure with arrays
  TYPE :: array_t
    INTEGER :: ints(2:5)
    REAL :: matrix(2, 2)
  END TYPE array_t

  ! Structure containing structures
  TYPE :: nestedStruct_t
    TYPE(basics_t) :: basics
    TYPE(array_t) :: arrays
  END TYPE nestedStruct_t

  TYPE(empty_t) :: empty
  TYPE(basics_t) :: basics
  TYPE(strings_t) :: strings
  TYPE(array_t) :: arrays
  TYPE(nestedStruct_t) :: nestedStruct

  empty = empty_t ()

  basics = basics_t (42, -1.5, (.5, .5), .FALSE.)
  IF (basics%i /= 42 .OR. basics%r /= -1.5 &
      .OR. basics%c /= (.5, .5) .OR. basics%l) THEN
    STOP 1
  END IF

  strings = strings_t ("hello", "abc", "this one is long")
  IF (strings%str1 /= "hello" .OR. strings%str2 /= "abc" &
      .OR. strings%long /= "this one i") THEN
    STOP 2
  END IF

  arrays = array_t ( (/ 1, 2, 3, 4 /), RESHAPE((/ 5, 6, 7, 8 /), (/ 2, 2 /)) )
  IF (arrays%ints(2) /= 1 .OR. arrays%ints(3) /= 2 &
      .OR. arrays%ints(4) /= 3 .OR. arrays%ints(5) /= 4 &
      .OR. arrays%matrix(1, 1) /= 5. .OR. arrays%matrix(2, 1) /= 6. &
      .OR. arrays%matrix(1, 2) /= 7. .OR. arrays%matrix(2, 2) /= 8.) THEN
    STOP 3
  END IF

  nestedStruct = nestedStruct_t (basics_t (42, -1.5, (.5, .5), .FALSE.), arrays)
  IF (nestedStruct%basics%i /= 42 .OR. nestedStruct%basics%r /= -1.5 &
      .OR. nestedStruct%basics%c /= (.5, .5) .OR. nestedStruct%basics%l &
      .OR. ANY(nestedStruct%arrays%ints /= arrays%ints) &
      .OR. ANY(nestedStruct%arrays%matrix /= arrays%matrix)) THEN
    STOP 4
  END IF

END PROGRAM test
