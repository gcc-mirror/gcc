! { dg-do compile }
function return_char1(i) bind(c,name='return_char1')
  use iso_c_binding
  implicit none
  integer(c_int) :: i
  character(c_char) :: j
  character(c_char) :: return_char1

  j = achar(i)
  return_char1 = j
end function return_char1
function return_char2(i) result(output) bind(c,name='return_char2')
  use iso_c_binding
  implicit none
  integer(c_int) :: i
  character(c_char) :: j
  character(c_char) :: output

  j = achar(i)
  output = j
end function return_char2
function return_char3(i) bind(c,name='return_char3') result(output)
  use iso_c_binding
  implicit none
  integer(c_int) :: i
  character(c_char) :: j
  character(c_char) :: output

  j = achar(i)
  output = j
end function return_char3
