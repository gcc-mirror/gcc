! Program to test the DOT_PRODUCT intrinsic
program testforall
   implicit none
   integer, dimension (3) :: a
   integer, dimension (3) :: b
   real, dimension(3) :: c
   real r
   complex, dimension (2) :: z1
   complex, dimension (2) :: z2
   complex z

   a = (/1, 2, 3/);
   b = (/4, 5, 6/);
   c = (/4, 5, 6/);

   if (dot_product(a, b) .ne. 32) STOP 1

   r = dot_product(a, c)
   if (abs(r - 32.0) .gt. 0.001) STOP 2

   z1 = (/(1.0, 2.0), (2.0, 3.0)/)
   z2 = (/(3.0, 4.0), (4.0, 5.0)/)
   z = dot_product (z1, z2)
   if (abs (z - (34.0, -4.0)) .gt. 0.001) STOP 3
end program
