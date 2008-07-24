! { dg-do compile }
! { dg-options "-std=f95 -Wintrinsics-std" }
!
! PR fortran/32778 - pedantic warning: intrinsics that 
!                    are GNU extensions not part of -std=gnu
!
! (2/3) Check for GNU extensions and intrinsics from F2003 if -std=f95.
!

CHARACTER(len=255) :: tmp
REAL(8) :: x

! GNU extension, check overload of F77 standard intrinsic
x = ZABS(CMPLX(0.0, 1.0, 8))    ! { dg-warning "extension" }

! GNU extension
CALL flush()                    ! { dg-warning "extension" }

! F95
tmp = ADJUSTL("  gfortran  ")

! F2003
CALL GET_COMMAND (tmp)          ! { dg-warning "Fortran 2003" }

END
