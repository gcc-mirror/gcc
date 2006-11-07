! { dg-do compile }
! { dg-shouldfail "VOLATILE not part of F95" }
! { dg-options "-std=f95" }
! Test whether volatile statements and attributes are rejected
! with -std=f95.
! PR fortran/29601
program volatile_test
  implicit none
  real, volatile :: foo ! { dg-error "VOLATILE attribute" }
  real :: l
  volatile :: l         ! { dg-error "VOLATILE statement" }
  l   = 4.0
  foo = 3.0             ! { dg-error "no IMPLICIT type" }   
end program volatile_test
