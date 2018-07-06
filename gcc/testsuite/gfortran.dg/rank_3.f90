! { dg-do compile }
! { dg-options "-std=f2008" }
!
! PR fortran/48820
!
intrinsic :: rank  ! { dg-error "new in Fortran 2018" }
end
