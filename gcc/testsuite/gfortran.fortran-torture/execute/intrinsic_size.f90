! Program to test the SIZE intrinsics
program testsize
   implicit none
   real, dimension(:, :), allocatable :: a
   integer, dimension(5) :: j
   integer, dimension(2, 3) :: b
   integer i

   if (size (b(2, :), 1) .ne. 3) STOP 1
   
   allocate (a(3:8, 5:7))

   ! With one parameter
   if (size(a) .ne. 18) STOP 2

   ! With two parameters, assigning to an array
   j = size(a, 1)
   if (any (j .ne. (/6, 6, 6, 6, 6/))) STOP 3

   ! With a variable second parameter
   i = 2
   i = size(a, i)
   if (i .ne. 3) STOP 4

   call test(a)
contains

subroutine test (a)
   real, dimension (1:, 1:) :: a
   integer i

   i = 2
   if ((size(a, 1) .ne. 6) .or. (size(a, i) .ne. 3)) STOP 5
   if (size (a) .ne. 18 ) STOP 6
end subroutine
end program

