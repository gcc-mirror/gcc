! Program to test procudure args
subroutine test (a, b)
   integer, intent (IN) :: a
   integer, intent (OUT) :: b

   if (a .ne. 42) call abort
   b = 43
end subroutine

program args
   implicit none
   external test
   integer i, j

   i = 42
   j = 0
   CALL test (i, j)
   if (i .ne. 42) call abort
   if (j .ne. 43) call abort
   i = 41
   CALL test (i + 1, j)
end program
