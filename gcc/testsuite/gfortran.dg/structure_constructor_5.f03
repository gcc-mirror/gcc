! { dg-do run }
! Structure constructor with default initialization.

PROGRAM test
  IMPLICIT NONE

  ! Type with all default values
  TYPE :: quasiempty_t
    CHARACTER(len=5) :: greeting = "hello"
  END TYPE quasiempty_t

  ! Structure of basic data types
  TYPE :: basics_t
    INTEGER :: i = 42
    REAL :: r
    COMPLEX :: c = (0., 1.)
  END TYPE basics_t

  TYPE(quasiempty_t) :: empty
  TYPE(basics_t) :: basics

  empty = quasiempty_t ()
  IF (empty%greeting /= "hello") THEN
    CALL abort()
  END IF

  basics = basics_t (r = 1.5)
  IF (basics%i /= 42 .OR. basics%r /= 1.5 .OR. basics%c /= (0., 1.)) THEN
    CALL abort()
  END IF

  basics%c = (0., 0.) ! So we see it's surely gotten re-initialized
  basics = basics_t (1, 5.1)
  IF (basics%i /= 1 .OR. basics%r /= 5.1 .OR. basics%c /= (0., 1.)) THEN
    CALL abort()
  END IF

END PROGRAM test
