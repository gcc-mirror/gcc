! { dg-do compile }
!
! Test the fix for PR103366.
!
! Contributed by Gerhardt Steinmetz  <gscfq@t-online.de>
!
program p
  call u([1])
contains
   subroutine s(x) bind(c)
      type(*) :: x(..)
   end
   subroutine u(x)
      class(*) :: x(..)
      call s(x)         ! Used to ICE here
   end
end
