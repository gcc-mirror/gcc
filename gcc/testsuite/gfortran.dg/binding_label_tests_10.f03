! { dg-do compile }
module binding_label_tests_10
  use iso_c_binding
  implicit none
  integer(c_int), bind(c,name="c_one") :: one
end module binding_label_tests_10
