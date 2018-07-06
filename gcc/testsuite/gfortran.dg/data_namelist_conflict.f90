! { dg-do run }
! Problem report: http://gcc.gnu.org/ml/fortran/2010-05/msg00139.html
!
module globals
   implicit none
   integer j
   data j/1/
end module

program test
   use globals
   implicit none
   character(len=80) str
   integer :: i
   data i/0/
   namelist /nl/i,j
   open(unit=10,status='scratch')
   write(10,nl)
   i = 42
   j = 42
   rewind(10)
   read(10,nl)
   if (i /= 0 .or. j /= 1) STOP 1
   close(10)
end program
