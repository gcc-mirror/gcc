! { dg-do run }
!
! Test the fix for pr92753
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
module m
   type t
      character(3) :: c
   end type
   type u
      complex :: z
   end type
   type(t), parameter :: x = t ('abc')
   integer, parameter :: l = x%c%len           ! Used to ICE

   type(u), parameter :: z = u ((42.0,-42.0))
end
program p
   use m
   call s (x%c%len)                            !   ditto

   if (int (z%z%re) .ne. 42) stop 1            ! Produced wrong code and
   if (int (z%z%re) .ne. -int (z%z%im)) stop 2 ! runtime seg fault
contains
   subroutine s(n)
      if (n .ne. l) stop 3
   end
end
