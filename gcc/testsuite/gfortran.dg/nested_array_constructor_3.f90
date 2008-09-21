! { dg-do run }

! PR fortran/35846
! Alternate test that also produced an ICE because of a missing length.

PROGRAM test
  IMPLICIT NONE
  CHARACTER(LEN=2) :: x

  x = 'a'
  CALL sub ( (/ TRIM(x), 'a' /) // 'c')
END PROGRAM

SUBROUTINE sub(str)
  IMPLICIT NONE
  CHARACTER(LEN=*) :: str(2)
  WRITE (*,*) str

  IF (str(1) /= 'ac' .OR. str(2) /= 'ac') THEN
    CALL abort ()
  END IF
END SUBROUTINE sub
