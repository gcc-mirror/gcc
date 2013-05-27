! { dg-do compile }
! This file must be compiled AFTER binding_label_tests_10.f03, which it 
! should be because dejagnu will sort the files.
module binding_label_tests_10_main
  use iso_c_binding
  implicit none
  integer(c_int), bind(c,name="c_one") :: one ! { dg-error "Variable one from module binding_label_tests_10 with binding label c_one at .1. uses the same global identifier as entity at .2. from module binding_label_tests_10_main" }
end module binding_label_tests_10_main

program main
  use binding_label_tests_10 ! { dg-error "Variable one from module binding_label_tests_10 with binding label c_one at .1. uses the same global identifier as entity at .2. from module binding_label_tests_10_main" }
  use binding_label_tests_10_main
end program main
