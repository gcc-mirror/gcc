! { dg-do compile }
! { dg-options "-Wunused" }
!
! PR fortran/31461
!
! Contributed by Vivek Rao.
!

module util_mod
  integer :: i,j
end module util_mod

program main
  use util_mod, only: i,j ! { dg-warning "Unused module variable .i. which has been explicitly imported" }
  j = 1
  print*,"j=",j
end program main

! { dg-final { cleanup-modules "util_mod" } }
