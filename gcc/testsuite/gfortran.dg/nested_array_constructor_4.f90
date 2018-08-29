! { dg-do run }

! PR fortran/35846
! Alternate test that also produced an ICE because of a missing length.

PROGRAM test
  IMPLICIT NONE
  CHARACTER(LEN=2) :: x
  INTEGER :: length

  x = 'a'
  length = LEN ( (/ TRIM(x), 'a' /) // 'c')

  IF (length /= 2) THEN
    STOP 1
  END IF
END PROGRAM
