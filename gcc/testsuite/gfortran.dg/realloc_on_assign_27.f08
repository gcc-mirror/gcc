! { dg-do run }

  type :: t
    integer :: i
  end type

  type, extends(t) :: r
    real :: r
  end type

  class(t), allocatable :: x
  type(r) :: y = r (3, 42)

  x = y
  if (x%i /= 3) STOP 1
  select type(x)
    class is (r)
      if (x%r /= 42.0) STOP 2
    class default
      STOP 3
  end select
end

