! { dg-do compile }
! { dg-options "-std=f2003" }
!
!
! This file depends on the module test_common_binding_labels_2.  That module
! must be compiled first and not be removed until after this test.
module test_common_binding_labels_2_main
  use, intrinsic :: iso_c_binding, only: c_double, c_int
  implicit none
  
  common /mycom/ r, s ! { dg-error "same binding name" }
  real(c_double) :: r
  real(c_double) :: s
  ! this next line is an error; if a common block is bind(c), the binding label
  ! for it must match across all scoping units that declare it.
  bind(c, name="my_common_block_2") :: /mycom/ 

  common /com2/ i ! { dg-error "same binding name" }
  integer(c_int) :: i
  bind(c, name="mycom2") /com2/
end module test_common_binding_labels_2_main

program main
  use test_common_binding_labels_2 ! { dg-error "same binding name" }
  use test_common_binding_labels_2_main ! { dg-error "same binding name" }
end program main
! { dg-final { cleanup-modules "test_common_binding_labels_2" } }
