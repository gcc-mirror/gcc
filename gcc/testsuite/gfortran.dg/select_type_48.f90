! { dg-do run }
!
! Test the fix for PR92976, in which the TYPE IS statement caused an ICE
! because of the explicit bounds of 'x'.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer :: i
   end type
   class(t), allocatable :: c(:)
   allocate (c, source = [t(1111),t(2222),t(3333)])
   call s(c)
   if (sum (c%i) .ne. 3333) stop 1
contains
   subroutine s(x)
      class(t) :: x(2)
      select type (x)
! ICE as compiler attempted to assign descriptor to an array
         type is (t)
            x%i = 0
! Make sure that bounds are correctly translated.
            call counter (x)
      end select
   end
   subroutine counter (arg)
     type(t) :: arg(:)
     if (size (arg, 1) .ne. 2) stop 2
   end
end
