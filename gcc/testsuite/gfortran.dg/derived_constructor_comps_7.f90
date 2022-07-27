! { dg-do run }
! PR fortran/104619

module m
  implicit none
  type :: item
     real :: x
  end type item
  type :: container
     type(item) :: items(3)
  end type container
end module

program p
  use m
  implicit none
  type(item), allocatable :: items(:)
  type(container) :: c
  integer         :: i, n
  items = [item(3.0), item(4.0), item(5.0)]
  c = container(items=[(items(i), i = 1, size(items))])
  if (any (c%items% x /= items% x)) stop 1
  n = size (items)
  c = container(items=[(items(i), i = 1, n)])
  if (any (c%items% x /= items% x)) stop 2
  c = container(items=[(items(i), i = 1, 3)])
  if (any (c%items% x /= items% x)) stop 3
end program
