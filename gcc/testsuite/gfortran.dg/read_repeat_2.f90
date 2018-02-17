! { dg-do run }
!
! PR fortran/56810
!
! Contributed by Jonathan Hogg
!
program test
   implicit none

   integer :: i
   complex :: a(4)

   open (99, status='scratch')
   write (99, *) '4*(1.0,2.0)'
   rewind (99)
   read (99,*) a(:)
   close (99)
   if (any (a /= cmplx (1.0,2.0))) STOP 1
end program test
