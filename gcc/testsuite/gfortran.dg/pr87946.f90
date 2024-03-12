! { dg-do run }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m
   type t
   contains
      generic :: h => g
      procedure, private :: g
   end type
contains
   function g(x, y) result(z)
      class(t), intent(in) :: x
      real, intent(in) :: y(:, :)
      real :: z(size(y, 2))
      integer :: i
      do i = 1, size(y, 2)
        z(i) = i
      end do
   end
end
module m2
   use m
   type t2
      class(t), allocatable :: u(:)
   end type
end
   use m2
   type(t2) :: x
   real :: y(1,5)
   allocate (x%u(1))
   if (any (int(f (x, y)) .ne. [1,2,3,4,5])) stop 1
   deallocate (x%u)
contains
   function f(x, y) result(z)
      use m2
      type(t2) :: x
      real :: y(:, :)
      real :: z(size(y, 2))
      z = x%u(1)%h(y)          ! Used to segfault here
   end
end
