! { dg-do run }
!
! Check pr57117 is fixed.

program pr57117
  implicit none

  type :: ti
    integer :: i
  end type

  class(ti), allocatable :: x(:,:), y(:,:)
  integer :: i

  allocate(x(2,6))
  select type (x)
    class is (ti)
       x%i = reshape([(i,i=1, 12)],[2,6])
  end select
  allocate(y, source=transpose(x))

  if (any( ubound(y) /= [6,2])) call abort()
  if (any(reshape(y(:,:)%i, [12]) /= [ 1,3,5,7,9,11, 2,4,6,8,10,12])) call abort()
  deallocate (x,y)
end

