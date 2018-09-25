! { dg-do  run }
! PR fortran/87401 - this used to segfault at runtime.
! Test case by Janus Weil.

program assoc_intent_out

   implicit none

   real :: r

   associate(o => r)
      call sub(o)
   end associate

contains

   subroutine sub(out)
      real, intent(out) :: out
      out = 0.0
   end subroutine

end

