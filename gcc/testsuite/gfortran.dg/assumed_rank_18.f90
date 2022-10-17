! { dg-do run }
! PR 91643 - this used to cause an ICE.
! Original test case by Gerhard Steinmetz.
program p
   real :: z(3) = [1.0, 2.0, 3.0]
   call g(z)
contains
   subroutine g(x)
      real :: x(..)
      select rank (x)
        rank (1)
          call h(x)
      end select
   end
   subroutine h(x)
      real :: x(*)
      if (x(1) /= 1.0) stop 1
   end
end
