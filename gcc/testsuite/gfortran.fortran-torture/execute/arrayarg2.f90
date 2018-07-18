! Program to test array arguments which depend on other array arguments
program arrayarg2
   integer, dimension(5) :: a, b

   a = (/1, 2, 3, 4, 5/)
   b = (/2, 3, 4, 5, 6/)
   
   call test (a, b)

   if (any (b .ne. (/4, 7, 10, 13, 16/))) STOP 1
contains
subroutine test (x1, x2)
   implicit none
   integer, dimension(1:), intent(in) :: x1
   integer, dimension(1:), intent(inout) :: x2
   integer, dimension(1:size(x1)) :: x3

   x3 = x1 * 2
   x2 = x2 + x3
end subroutine test
end program
