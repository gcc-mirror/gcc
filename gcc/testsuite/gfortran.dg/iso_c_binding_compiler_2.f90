! { dg-do compile }
! { dg-options "-std=f2003" }
!
! PR fortran/40569
!
! Check compiler_version/compiler_options intrinsics
!
use iso_fortran_env, only:  compiler_options ! { dg-error "is not in the selected standard" }
use iso_fortran_env, only:  compiler_version ! { dg-error "is not in the selected standard" }
  implicit none
end
