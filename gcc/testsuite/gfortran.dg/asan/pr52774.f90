! { dg-do run }
!
! PR 52774

module m
  type po
    real(8), allocatable :: p(:)
  end type
  type wrap
    type(po) :: c
    integer :: k = 0
  end type
contains
  function toto (dd) result (y)
    type(po) :: dd, y
    deallocate (dd%p)
    dd%p = [1d0, 2d0]
    y = dd
  end function
  function parent (w) result (y)
    type(wrap) :: w
    type(po) :: y
    deallocate (w%c%p)
    w%c%p = [3d0]
    y = w%c
  end function
  function child (c) result (y)
    type(po) :: c
    type(wrap) :: y
    deallocate (c%p)
    c%p = [4d0, 5d0]
    y%c = c
    y%k = 6
  end function
end module

program p
  use m
  type(po) :: p1
  type(wrap) :: w
  allocate (p1%p(1))
  p1 = toto (p1)
  if (any (p1%p /= [1d0, 2d0])) stop 1
  allocate (w%c%p(2))
  w%c = parent (w)
  if (any (w%c%p /= [3d0])) stop 2
  w = child (w%c)
  if (any (w%c%p /= [4d0, 5d0]) .or. w%k /= 6) stop 3
end program
