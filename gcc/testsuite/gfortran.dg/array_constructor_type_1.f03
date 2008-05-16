! { dg-do run }
!
! PR fortran/27997
!
! Simple array constructor with typespec.
!
PROGRAM test
  IMPLICIT NONE
  INTEGER :: array(5)

  array = (/ INTEGER :: 18, 12, 31, 3, 42.4 /)

  IF (array(1) /= 18 .OR. array(2) /= 12 .OR. &
      array(3) /= 31 .OR. array(4) /=  3 .OR. array(5) /= 42) THEN
      CALL abort()
  END IF
END PROGRAM test
