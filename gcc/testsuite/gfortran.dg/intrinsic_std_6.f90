! { dg-do compile }
! { dg-options "-std=f95 -Wintrinsics-std -fdump-tree-original" }

!
! See intrinsic_std_1.f90 for more compile-time checks
!

! PR fortran/33141
! Check for the expected behaviour when an intrinsic function/subroutine is
! called that is not available in the defined standard or that is a GNU
! extension:
! There should be a warning emitted on the call, and the reference should be
! treated like an external call.
! For declaring a non-standard intrinsic INTRINSIC, a hard error should be
! generated, of course.

SUBROUTINE no_implicit
  IMPLICIT NONE
  REAL :: asinh ! { dg-warning "Fortran 2008" }

  ! abort is a GNU extension
  CALL abort () ! { dg-warning "extension" }

  ! ASINH is an intrinsic of F2008
  ! The warning should be issued in the declaration above where it is declared
  ! EXTERNAL.
  WRITE (*,*) ASINH (1.) ! { dg-warning "Fortran 2008" }
END SUBROUTINE no_implicit

SUBROUTINE implicit_type
  ! acosh has implicit type

  WRITE (*,*) ACOSH (1.) ! { dg-warning "Fortran 2008" }
  WRITE (*,*) ACOSH (1.) ! { dg-bogus "Fortran 2008" }
END SUBROUTINE implicit_type

! Scan that really external functions are called.
! { dg-final { scan-tree-dump " abort " "original" } }
! { dg-final { scan-tree-dump " asinh " "original" } }
! { dg-final { scan-tree-dump " acosh " "original" } }
! { dg-final { cleanup-tree-dump "original" } }
