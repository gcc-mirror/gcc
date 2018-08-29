! Program to test the shape intrinsic
program testbounds
   implicit none
   real, dimension(:, :), allocatable :: a
   integer, dimension(2) :: j
   integer i

   allocate (a(3:8, 6:7))

   j = shape (a);
   if (any (j .ne. (/ 6, 2 /))) STOP 1

   call test(a)
contains

subroutine test (a)
   real, dimension (1:, 1:) :: a

   if (any (shape (a) .ne. (/ 6, 2 /))) STOP 2
end subroutine
end program

