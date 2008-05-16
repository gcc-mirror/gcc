! { dg-do run }
!
! PR fortran/27997
!
! Test empty array constructor with typespec.
!
PROGRAM test
  IMPLICIT NONE
  INTEGER :: array(2)

  array = (/ 5, [INTEGER ::], 6 /)

  IF (array(1) /= 5 .OR. array(2) /= 6) THEN
      CALL abort()
  END IF
END PROGRAM test
