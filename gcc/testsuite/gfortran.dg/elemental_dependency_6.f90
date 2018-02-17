! { dg-do run }
!
! PR fortran/66089
! Check that we do create a temporary for C(1) below in the assignment
! to C.

  type :: t
    integer :: c
  end type t

  type(t), dimension(5) :: b, c

  b = t(7)
  c = t(13)
  c = plus(c(1), b)
! print *, c
  if (any(c%c /= 20)) STOP 1

contains

  elemental function plus(lhs, rhs)
    type(t), intent(in) :: lhs, rhs
    type(t)             :: plus
    plus%c = lhs%c + rhs%c
  end function plus

end
