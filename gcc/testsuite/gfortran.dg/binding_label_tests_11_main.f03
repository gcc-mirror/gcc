! { dg-do compile }
! { dg-compile-aux-modules "binding_label_tests_11.f03" }
module binding_label_tests_11_main
  use iso_c_binding, only: c_int
  implicit none
contains
  function one() bind(c, name="c_one") ! { dg-error "Procedure 'one' with binding label 'c_one' at .1. uses the same global identifier as entity at .2." }
    integer(c_int) one
    one = 1
  end function one
end module binding_label_tests_11_main

program main
  use binding_label_tests_11 ! { dg-error "Procedure 'one' with binding label 'c_one' at .1. uses the same global identifier as entity at .2." }
  use binding_label_tests_11_main
end program main
