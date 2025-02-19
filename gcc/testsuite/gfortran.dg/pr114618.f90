! { dg-do run }
! PR114618 Format produces incorrect output when contains 1x, ok when uses " " 
! aside: Before patch output1 is garbage.
program pr114618
   implicit none
   integer, parameter :: wp = kind(0d0)
   real(kind=wp) :: pi  = 3.14159265358979323846264338_wp
   character(len=*), parameter:: fmt1 = '(19("."),t1,g0,1x,t21,g0)'
   character(len=*), parameter:: fmt2 = '(19("."),t1,g0," ",t21,g0)'
   character(21) :: output1, output2
   write (output1, fmt1) 'RADIX', radix(pi)
   write (output2, fmt2) 'RADIX', radix(pi)
   if (output1 /= 'RADIX.............. 2') stop 1
   if (output2 /= 'RADIX ............. 2') stop 2
end program pr114618
