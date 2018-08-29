! { dg-do run }
!
! Testcase for pr57117

implicit none

  type :: ti
    integer :: i
  end type

  class(ti), allocatable :: x(:,:), z(:)
  integer :: i

  allocate(x(3,3))
  x%i = reshape([( i, i = 1, 9 )], [3, 3])
  allocate(z(9), source=reshape(x, (/ 9 /)))

  if (any( z%i /= [( i, i = 1, 9 )])) STOP 1
  deallocate (x, z)
end

