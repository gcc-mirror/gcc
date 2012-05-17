! { dg-do compile }
module x
  use iso_c_binding
  implicit none
contains
  function bar() bind(c) ! { dg-error "cannot be an array" }
    integer(c_int) :: bar(5)
  end function bar

  function my_string_func() bind(c) ! { dg-error "cannot be a character string" }
    character(kind=c_char, len=10) :: my_string_func
    my_string_func = 'my_string' // C_NULL_CHAR
  end function my_string_func
end module x
