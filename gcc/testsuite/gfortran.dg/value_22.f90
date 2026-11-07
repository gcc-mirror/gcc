! { dg-do run }
!
! PR 51284
! A VALUE dummy of a PDT with an allocatable component is passed by
! reference to a copy, like that of any type with allocatable components.

module m
  implicit none
  type :: pt(k)
    integer, kind :: k
    integer(k), allocatable :: v(:)
  end type
contains
  subroutine sp (x)
    type(pt(4)), value :: x
    if (any (x%v /= [1, 2])) stop 1
    x%v = 99
  end subroutine
end module

program p
  use m
  implicit none
  type(pt(4)) :: z
  z%v = [1, 2]
  call sp (z)
  if (any (z%v /= [1, 2])) stop 2
end program
