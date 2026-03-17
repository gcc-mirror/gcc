! { dg-do run }
! Test fix for PR105168, in which nterface mapping was failing with CLASS 'x'
! and parentheses around the actual argument.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m
   type t
      integer :: a
   contains
      final :: final_t
   end type
   integer :: cntr = 0
contains
   function f(x, factor) result(z)
      class(t) :: x(:) ! Worked, with or without parentheses if s/CLASS/TYPE/
      type(t) :: z(2)
      integer :: factor
      z = x            ! Seg fault here
      z%a = factor * z%a
   end
   impure elemental subroutine final_t (arg)
      type (t), intent(in) :: arg
      cntr = cntr + 1
   end subroutine
end module
program p
   use m
   class(t), allocatable :: y(:), z(:)
   y = [t(2),t(4)]
   allocate (t :: z(2))
   z = f((y), 1)          ! Failed even with parentheses removed
   if (any(z%a /= [2,4])) stop 1
   z = f(y, 2)            ! Failed but now OK
   if (any (z%a /= [4,8])) stop 2
   deallocate (y, z)
   if (cntr /= 16) stop 3 ! 6 for each assignment and 4 for deallocation
end
