! Program to test the MATMUL intrinsic
program intrinsic_matmul
   implicit none
   integer, dimension(2, 3) :: a
   integer, dimension(3, 2) :: b
   integer, dimension(2) :: x
   integer, dimension(3) :: y
   integer, dimension(2, 2) :: r
   integer, dimension(3) :: v
   real, dimension (2,2) :: aa
   real, dimension (4,2) :: cc

   a = reshape((/1, 2, 2, 3, 3, 4/), (/2, 3/))
   b = reshape((/1, 2, 3, 3, 4, 5/), (/3, 2/))
   x = (/1, 2/)
   y = (/1, 2, 3/)

   r = matmul(a, b)
   if (any(r .ne. reshape((/14, 20, 26, 38/), (/2, 2/)))) call abort

   v = matmul(x, a)
   if (any(v .ne. (/5, 8, 11/))) call abort

   v(1:2) = matmul(a, y)
   if (any(v(1:2) .ne. (/14, 20/))) call abort

  aa = reshape((/ 1.0, 1.0, 0.0, 1.0/), shape(aa))
  cc = 42.
  cc(1:2,1:2) = matmul(aa, transpose(aa))
  if (any(cc(1:2,1:2) .ne. reshape((/ 1.0, 1.0, 1.0, 2.0 /), (/2,2/)))) call abort
  if (any(cc(3:4,1:2) .ne. 42.)) call abort
end program
