!{ dg-do compile }

! Check PR120847 is fixed.

program p
  implicit none

  type T
    integer, allocatable :: i(:, :) [:]
  end type T

  type(T) :: o
  integer, allocatable :: c[:]
  integer :: i

  c = 7

  allocate(o%i(4, 5)[*], source=6)

  do i = 1, 4
    c = o%i(mod(i, 2), mod(i, 3))[1]
  end do

end program p
