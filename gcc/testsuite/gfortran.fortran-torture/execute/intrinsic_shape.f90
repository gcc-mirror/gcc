! Program to test the shape intrinsic
program testbounds
   implicit none
   real, dimension(:, :), allocatable :: a
   integer, dimension(2) :: j
   integer i

   allocate (a(3:8, 6:7))

   j = shape (a);
   if (any (j .ne. (/ 6, 2 /))) call abort

   call test(a)
contains

subroutine test (a)
   real, dimension (1:, 1:) :: a

   if (any (shape (a) .ne. (/ 6, 2 /))) call abort
end subroutine
end program

