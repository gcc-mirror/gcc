! { dg-do run }
!
! PR fortran/27997
!
! Ensure that :: is present when a typespec is deduced.
!
PROGRAM test
  INTEGER :: array(1)
  INTEGER = 42

  array = [ INTEGER ]
  IF (array(1) /= 42) THEN
    STOP 1
  END IF
END PROGRAM test
