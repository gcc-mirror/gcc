! Test implicit character declarations.
! This requires some coordination between the typespec and variable name range
! matchers to get it right.
module implicit_1
  integer, parameter :: x = 10
  integer, parameter :: y = 6
  integer, parameter :: z = selected_int_kind(4)
end module
subroutine foo(n)
  use implicit_1
  ! Test various combinations with and without character length
  ! and type kind specifiers
  implicit character(len=5) (a)
  implicit character(n) (b)
  implicit character*6 (c-d)
  implicit character (e)
  implicit character(x-y) (f)
  implicit integer(z) (g)
  implicit character (z)

  a1 = 'Hello'
  b1 = 'world'
  c1 = 'wibble'
  d1 = 'hmmm'
  e1 = 'n'
  f1 = 'test'
  g1 = 1
  x1 = 1.0
  y1 = 2.0
  z1 = 'A'
end

