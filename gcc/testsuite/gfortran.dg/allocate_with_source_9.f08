! { dg-do run }
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>,
!                Andre Vehreschild  <vehre@gcc.gnu.org>

program main

  type T
     integer, allocatable :: acc(:)
  end type

  integer :: n, lb, ub
  integer :: vec(9)
  type(T) :: o1, o2
  vec = [(i, i= 1, 9)]
  n = 42
  lb = 7
  ub = lb + 2
  allocate(o1%acc, source=vec)
  allocate(o2%acc, source=o1%acc(lb:ub))
  if (any (o2%acc /= [7, 8, 9])) call abort()
  block
    real, dimension(0:n) :: a
    real, dimension(:), allocatable :: c
    call random_number(a)
    allocate(c,source=a(:))
    if (any (abs(a - c) > 1E-6)) call abort()
  end block
end program main
