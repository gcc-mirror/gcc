! { dg-do compile }
! { dg-options "-Wnonstd-intrinsics -std=f2003" }
!
! PR fortran/32778 - pedantic warning: intrinsics that 
!                    are GNU extensions not part of -std=gnu
!
! (3/3) Check for GNU extensions if -std=f2003.
!

CHARACTER(len=255) :: tmp
REAL(8) :: x

! GNU extension, check overload of F77 standard intrinsic
x = ZABS(CMPLX(0.0, 1.0, 8))    ! { dg-error "is not included in the selected standard" }

! GNU extension
CALL flush()                    ! { dg-error "is not included in the selected standard" }

! F95
tmp = ADJUSTL("  gfortran  ")

! F2003
CALL GET_COMMAND (tmp)

END
