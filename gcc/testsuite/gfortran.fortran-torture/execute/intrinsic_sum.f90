! Program to test the FORALL construct
program testforall
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   logical, dimension (3, 3) :: m
   integer i

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/));

   if (sum(a) .ne. 45) call abort
   b = sum (a, 1)
   if (b(1) .ne. 6) call abort
   if (b(2) .ne. 15) call abort
   if (b(3) .ne. 24) call abort

   m = .true.
   m(1, 1) = .false.
   m(2, 1) = .false.

   if (sum (a, mask=m) .ne. 42) call abort
   b = sum (a, 2, m)
   if (b(1) .ne. 11) call abort
   if (b(2) .ne. 13) call abort
   if (b(3) .ne. 18) call abort
end program
