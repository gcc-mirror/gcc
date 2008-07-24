! { dg-do run }
! { dg-options "-std=f95 -Wno-intrinsics-std" }

! PR fortran/33141
! Check that calls to intrinsics not in the current standard are "allowed" and
! linked to external procedures with that name.
! Addionally, this checks that -Wno-intrinsics-std turns off the warning.

SUBROUTINE abort ()
  IMPLICIT NONE
  WRITE (*,*) "Correct"
END SUBROUTINE abort

REAL FUNCTION asinh (arg)
  IMPLICIT NONE
  REAL :: arg

  WRITE (*,*) "Correct"
  asinh = arg
END FUNCTION asinh

SUBROUTINE implicit_none
  IMPLICIT NONE
  REAL :: asinh ! { dg-bogus "Fortran 2008" }
  REAL :: x

  ! Both times our version above should be called
  CALL abort () ! { dg-bogus "extension" }
  x = ASINH (1.) ! { dg-bogus "Fortran 2008" }
END SUBROUTINE implicit_none

SUBROUTINE implicit_type
  ! ASINH has implicit type here
  REAL :: x

  ! Our version should be called
  x = ASINH (1.) ! { dg-bogus "Fortran 2008" }
END SUBROUTINE implicit_type

PROGRAM main
  ! This should give a total of three "Correct"s
  CALL implicit_none ()
  CALL implicit_type ()
END PROGRAM main

! { dg-output "Correct\.*Correct\.*Correct" }
