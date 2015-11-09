! { dg-do run }
! PR fortran/68053
! Original code contributed by Gerhard Steinmetz
! <gerhard dot steinmetx dot fortran at t-online dot de>
program p
   integer, parameter :: n(3) = [1,2,3]
   integer, parameter :: x(1) = 7
   integer, parameter :: z(n(2):*) = x
   if (lbound(z,1) /= 2) call abort
end
