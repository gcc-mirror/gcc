! { dg-do run }
! PR 127731
! A pointer array function result used as a value is indexed by its span.

program p
  implicit none
  type t
    real :: x, y
  end type
  complex, target :: z(3) = [(1., 2.), (3., 4.), (5., 6.)]
  type(t), target :: d(3) = [t(1., 2.), t(3., 4.), t(5., 6.)]
  real, pointer :: r(:)
  class(*), pointer :: q(:)

  if (any (g() /= [2., 4., 6.])) stop 1
  if (sum (g()) /= 12.) stop 2
  if (any (gd() /= [2., 4., 6.])) stop 3
  if (any (gr() /= [1., 3., 5.])) stop 4

  associate (a => g())
    if (any (a /= [2., 4., 6.])) stop 5
  end associate

  call show (g(), 6)

  q => f()
  select type (q)
  type is (real)
    if (any (q /= [2., 4., 6.])) stop 7
  class default
    stop 8
  end select

  select type (s => f())
  type is (real)
    if (any (s /= [2., 4., 6.])) stop 9
  class default
    stop 10
  end select

  r => g()
  if (any (r /= [2., 4., 6.])) stop 11

contains

  function g() result(res)
    real, pointer :: res(:)
    res => z%im
  end function

  function gr() result(res)
    real, pointer :: res(:)
    res => z%re
  end function

  function gd() result(res)
    real, pointer :: res(:)
    res => d%y
  end function

  function f() result(res)
    class(*), pointer :: res(:)
    res => z%im
  end function

  subroutine show (a, n)
    real, intent(in) :: a(:)
    integer, intent(in) :: n
    if (any (a /= [2., 4., 6.])) stop n
  end subroutine

end program
