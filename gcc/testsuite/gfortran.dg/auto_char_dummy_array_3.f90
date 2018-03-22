! { dg-do run }

! PR fortran/49885
! Check that character arrays with non-constant char-length are handled
! correctly.

! Contributed by Daniel Kraft <d@domob.eu>,
! based on original test case and variant by Tobias Burnus in comment 2.

PROGRAM main
  IMPLICIT NONE

  CALL s (10)
      
CONTAINS

  SUBROUTINE s (nb)
    INTEGER :: nb
    CHARACTER(MAX (80, nb)) :: bad_rec(1)

    bad_rec(1)(1:2) = 'abc'
    IF (bad_rec(1)(1:2) /= 'ab') STOP 1
  END SUBROUTINE s

END PROGRAM main
