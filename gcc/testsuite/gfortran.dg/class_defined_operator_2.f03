! { dg-do run }
!
! Test the fix for PR99124 which used to ICE as shown.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m
   type t
      integer :: i
   contains
      procedure :: f
      generic :: operator(+) => f
   end type
contains
   elemental function f(a, b) result(c)
      class(t), intent(in) :: a, b
      type(t) :: c
      c = t(a%i + b%i)
   end
end
program p
   use m
   class(t), allocatable :: x(:), y(:), z
   allocate (x, source = [t(1), t(2)])
   allocate (y, source = [t(1), t(2)])
   x = x(2) + y                               ! ICE
   if (any (x%i .ne. [3, 4])) stop 1
   z = x(1)
   x = z + y                                  ! ICE
   if (any (x%i .ne. [4, 5])) stop 2
end
