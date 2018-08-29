! Program to test compilation of subroutines following the main program
program mainsub
   implicit none
   integer i
   external test

   i = 0
   call test (i)
   if (i .ne. 42) STOP 1
end program

subroutine test (p)
   implicit none
   integer p

   p = 42
end subroutine
