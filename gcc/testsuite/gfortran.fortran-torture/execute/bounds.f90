! Program to test the upper and lower bound intrinsics
program testbounds
   implicit none
   real, dimension(:, :), allocatable :: a
   integer, dimension(5) :: j
   integer i

   ! Check compile time simplification
   if (lbound(j,1).ne.1 .or. ubound(j,1).ne.5) call abort ()

   allocate (a(3:8, 6:7))

   ! With one parameter
   j = 0;
   j(3:4) = ubound(a)
   if (j(3) .ne. 8) call abort
   if (j(4) .ne. 7) call abort

   ! With two parameters, assigning to an array
   j = lbound(a, 1)
   if ((j(1) .ne. 3) .or. (j(5) .ne. 3)) call abort

   ! With a variable second parameter
   i = 2
   i = lbound(a, i)
   if (i .ne. 6) call abort

   call test(a)
contains
subroutine test (a)
   real, dimension (1:, 1:) :: a
   integer i

   i = 2
   if ((ubound(a, 1) .ne. 6) .or. (ubound(a, i) .ne. 2)) call abort
end subroutine
end program

