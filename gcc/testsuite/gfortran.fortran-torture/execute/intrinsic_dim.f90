! Program to test the DIM intrinsic
program intrinsic_dim
   implicit none
   integer i, j
   real(kind=4) :: r, s
   real(kind=8) :: p, q

   i = 1
   j = 4
   if (dim (i, j) .ne. 0) STOP 1
   if (dim (j, i) .ne. 3) STOP 2
   r = 1.0
   s = 4.0
   if (dim (r, s) .ne. 0.0) STOP 3
   if (dim (s, r) .ne. 3.0) STOP 4
   p = 1.0
   q = 4.0
   if (dim (p, q) .ne. 0.0) STOP 5
   if (dim (q, p) .ne. 3.0) STOP 6
end program
