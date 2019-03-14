! { dg-do run }
! PR fortran/66089 - used to ICE and, after that ICE was fixed,
! gave wrong results.
  type :: t
    integer :: c
  end type t

  class(t), dimension(:), allocatable :: b,c

  allocate (b(5), source=t(7))
  allocate(c(5), source=t(13))
  c = plus(c(1), b)
  if (any(c%c /= 20)) stop 1
  c = t(13)
  c = plus(b, c(1))
  if (any(c%c /= 20)) stop 2
contains

  elemental function plus(lhs, rhs)
    class(t), intent(in) :: lhs, rhs
    type(t)             :: plus
    plus%c = lhs%c + rhs%c
  end function plus

end
