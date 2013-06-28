! { dg-do compile }
! This file must be compiled AFTER binding_label_tests_13.f03, which it 
! should be because dejagnu will sort the files.  The module file 
! binding_label_tests_13.mod can not be removed until after this test is done.
module binding_label_tests_13_main
  use, intrinsic :: iso_c_binding, only: c_int
  integer(c_int) :: c3  ! { dg-error "Variable c3 from module binding_label_tests_13_main with binding label c3 at .1. uses the same global identifier as entity at .2. from module binding_label_tests_13" }
  bind(c) c3

contains
  subroutine c_sub() BIND(c, name = "C_Sub")
    use binding_label_tests_13 ! { dg-error "Variable c3 from module binding_label_tests_13_main with binding label c3 at .1. uses the same global identifier as entity at .2. from module binding_label_tests_13" }
  end subroutine c_sub
end module binding_label_tests_13_main
! { dg-final { cleanup-modules "binding_label_tests_13" } }
