! { dg-do compile }
! { dg-options "-fcoarray=lib -lcaf_single" }
!
! Check that reffing x on both sides of a coarray send does not ICE. 
! PR 85507

program check_dependency
  integer :: x[*]
  x[42] = x
end program check_dependency

