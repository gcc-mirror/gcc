! Program to test array IO.  Should print the numbers 1-20 in order
program arrayio
   implicit none
   integer, dimension(5, 4) :: a
   integer i, j

   do j=1,4
      a(:, j) = (/ (i + (j - 1) * 5, i=1,5) /)
   end do

   write (*,*) a
end program
