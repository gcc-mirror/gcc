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
  if (x%i /= 3) call abort()
  select type(x)
    class is (r)
      if (x%r /= 42.0) call abort()
    class default
      call abort()
  end select
end

