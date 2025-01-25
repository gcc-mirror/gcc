! { dg-do compile }
! { dg-additional-options "-Wunused-variable" }
!
! PR fortran/58857

BLOCK DATA valid
  integer  :: i
  integer  :: n  ! { dg-warning "not in a COMMON block" }
  class(*) :: zz ! { dg-warning "not in a COMMON block" }
  pointer  :: zz
  common /com/ i, r
END BLOCK DATA valid
