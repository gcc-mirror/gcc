! { dg-do run }
! PR fortran/61775.f90
program pi
   real, allocatable :: x(:)
   integer :: n
   n  = 10000
   x = [ (i,i=1,n) ]
   if (x(n) /= 10000) stop 1
end program pi
