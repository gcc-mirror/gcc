! { dg-do run }
! PR fortran/67885
! Original code contributed by Gerhard Steinmetz
! gerhard dot steinmetz dot fortran at t-online dot de
program p
   block
      integer, parameter :: a(2) = [1, 2]
      integer :: x(2)
      x = a
      if (x(1) /= 1) call abort
   end block
end
