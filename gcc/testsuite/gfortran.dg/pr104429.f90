! { dg-do run }
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m
   type t
      real :: r
   contains
      procedure :: op
      procedure :: assign
      generic :: operator(*) => op
      generic :: assignment(=) => assign
   end type
contains
   function op (x, y)
      class(t), allocatable :: op
      class(t), intent(in) :: x
      real, intent(in) :: y
      allocate (op, source = t (x%r * y))
   end
   subroutine assign (z, x)
      type(t), intent(in) :: x
      class(t), intent(out) :: z
      z%r = x%r
   end
end
program p
   use m
   class(t), allocatable :: x
   real :: y = 2
   allocate (x, source = t (2.0))
   x = x * y
   if (int (x%r) .ne. 4) stop 1
   if (allocated (x)) deallocate (x)
end
