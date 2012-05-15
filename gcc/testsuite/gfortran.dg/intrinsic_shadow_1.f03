! { dg-do compile }
! { dg-options "-std=f2003 -Wintrinsic-shadow" }

! PR fortran/33141
! Check that the expected warnings are emitted if a user-procedure has the same
! name as an intrinsic, but only if it is matched by the current -std=*.

MODULE testmod
  IMPLICIT NONE

CONTAINS

  ! ASIN is an intrinsic
  REAL FUNCTION asin (arg) ! { dg-warning "shadow the intrinsic" }
    IMPLICIT NONE
    REAL :: arg
  END FUNCTION asin

  ! ASINH is one but not in F2003
  REAL FUNCTION asinh (arg) ! { dg-bogus "shadow the intrinsic" }
    IMPLICIT NONE
    REAL :: arg
  END FUNCTION asinh

END MODULE testmod

! ACOS is an intrinsic
REAL FUNCTION acos (arg) ! { dg-warning "of an intrinsic" }
  IMPLICIT NONE
  REAL :: arg
END FUNCTION acos

! ACOSH not for F2003
REAL FUNCTION acosh (arg) ! { dg-bogus "of an intrinsic" }
  IMPLICIT NONE
  REAL :: arg
END FUNCTION acosh

! A subroutine with the same name as an intrinsic subroutine
SUBROUTINE random_number (arg) ! { dg-warning "of an intrinsic" }
  IMPLICIT NONE
  REAL, INTENT(OUT) :: arg
END SUBROUTINE random_number

! But a subroutine with the name of an intrinsic function is ok.
SUBROUTINE atan (arg) ! { dg-bogus "of an intrinsic" }
  IMPLICIT NONE
  REAL :: arg
END SUBROUTINE atan

! As should be a function with the name of an intrinsic subroutine.
REAL FUNCTION random_seed () ! { dg-bogus "of an intrinsic" }
END FUNCTION random_seed

! We do only compile, so no main program needed.
