! PR fortran/107706
!
! Contributed by G. Steinmetz
!

integer function f(i)
   implicit none
   !$omp assumes holds(i < g())  ! { dg-error "HOLDS expression at .1. must be a scalar logical expression" }
   integer, value :: i

   !$omp assume holds(i < g())  ! { dg-error "HOLDS expression at .1. must be a scalar logical expression" }
   block
   end block
   f = 3
contains
   function g()
      integer :: g(2)
      g = 4
   end
end
