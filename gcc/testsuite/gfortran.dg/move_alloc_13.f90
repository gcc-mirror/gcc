! { dg-do run }
!
! PR fortran/51970
! PR fortran/51977
!
type t
end type t
type, extends(t) :: t2
  integer :: a
end type t2

class(t), allocatable :: y(:), z(:)

allocate(y(2), source=[t2(2), t2(3)])
call func2(y,z)

select type(z)
  type is(t2)
    if (any (z(:)%a /= [2, 3])) STOP 1
  class default
    STOP 2
end select

contains
  function func(x)
   class (t), allocatable :: x(:), func(:)
   call move_alloc (x, func)
  end function

  function func1(x)
   class (t), allocatable :: x(:), func1(:)
   call move_alloc (func1, x)
  end function

  subroutine func2(x, y)
   class (t), allocatable :: x(:), y(:)
   call move_alloc (x, y)
  end subroutine
end
