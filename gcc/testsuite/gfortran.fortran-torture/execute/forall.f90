! Program to test the FORALL construct
program testforall
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   integer i

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/));

   forall (i=1:3)
      b(i) = sum (a(:, i))
   end forall

   if (b(1) .ne. 6) STOP 1
   if (b(2) .ne. 15) STOP 2
   if (b(3) .ne. 24) STOP 3
end program
