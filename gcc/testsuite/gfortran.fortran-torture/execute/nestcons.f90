! Program to test array expressions in array constructors.
program nestcons
   implicit none
   integer, parameter :: w1(3)= (/ 5, 6, 7/)
   integer, dimension(6) :: w2
   
   w2 = (/ 1, 2, w1(3:1:-1), 3 /)
   if (any (w2 .ne. (/ 1, 2, 7, 6, 5, 3/))) STOP 1
end
