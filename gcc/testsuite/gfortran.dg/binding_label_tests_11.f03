! { dg-do compile }
module binding_label_tests_11
  use iso_c_binding, only: c_int
  implicit none
contains
  function one() bind(c, name="c_one")
    integer(c_int) one
    one = 1
  end function one
end module binding_label_tests_11
