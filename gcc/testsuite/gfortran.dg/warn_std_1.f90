! { dg-do compile }
! { dg-options "-Wnonstd-intrinsics -std=gnu" }
!
! PR fortran/32778 - pedantic warning: intrinsics that 
!                    are GNU extensions not part of -std=gnu
!
! (1/3) Check for excess errors if -std=gnu.
!

CHARACTER(len=255) :: tmp
REAL(8) :: x

! GNU extension, check overload of F77 standard intrinsic
x = ZABS(CMPLX(0.0, 1.0, 8))

! GNU extension
CALL flush()

! F95
tmp = ADJUSTL("  gfortran  ")

! F2003
CALL GET_COMMAND (tmp)

END
