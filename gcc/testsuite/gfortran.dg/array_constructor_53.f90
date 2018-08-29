! { dg-do  run }
! PR 84931 - long array constructors with type conversion were not
! handled correctly. array_constructor_52.f90 tests the original
! problem.
program test
   implicit none
   integer, parameter :: n = 2**16 + 1
   real, dimension(n) :: y
   real, dimension(2*n) :: z
   integer :: i

   y = [33, (1, i=1, n-1) ]    ! Check that something more complicated works
   if (int(y(3)) /= 1) stop 1

   z = [[(1, i=1, n) ],[(2, i=1, n) ]] ! Failed with first version of the fix

   if (int(z(2)) /= 1) stop 2
   if (int(z(n+1)) /= 2) stop 3
end program test
