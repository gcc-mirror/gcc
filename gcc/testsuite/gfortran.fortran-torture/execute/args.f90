! Program to test procudure args
subroutine test (a, b)
   integer, intent (IN) :: a
   integer, intent (OUT) :: b

   if (a .ne. 42) STOP 1
   b = 43
end subroutine

program args
   implicit none
   external test
   integer i, j

   i = 42
   j = 0
   CALL test (i, j)
   if (i .ne. 42) STOP 2
   if (j .ne. 43) STOP 3
   i = 41
   CALL test (i + 1, j)
end program
