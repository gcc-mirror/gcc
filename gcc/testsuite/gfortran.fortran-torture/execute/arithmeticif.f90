! Program to test the arithmetic if statement
function testif (a)
   implicit none
   integer a, b, testif

   if (a) 1, 2, 3
   b = 2
   goto 4
 1 b = -1
   goto 4
 2 b = 0
   goto 4
 3 b = 1
 4 testif = b
end function

program testwrite
   implicit none
   integer i
   integer testif

   if (testif (-10) .ne. -1) call abort
   if (testif (0) .ne. 0) call abort
   if (testif (10) .ne. 1) call abort
end program
