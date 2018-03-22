! Program to test 
subroutine test (p)
   integer, dimension (3) :: p

   if (any (p .ne. (/ 2, 4, 6/))) STOP 1
end subroutine

program partparm
   implicit none
   integer, dimension (2, 3) :: a
   external test

   a = reshape ((/ 1, 2, 3, 4, 5, 6/), (/ 2, 3/))
   call test (a(2, :))
end program
