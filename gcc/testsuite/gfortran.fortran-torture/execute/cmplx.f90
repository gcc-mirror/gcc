! Test complex munbers
program testcmplx
   implicit none
   complex(kind=4) c, d
   complex(kind=8) z
   real(kind=4) x, y
   real(kind=8) q

   ! cmplx intrinsic
   x = 3
   y = 4
   c = cmplx(x,y)
   if (c .ne. (3.0, 4.0)) call abort
   x = 4
   y = 3
   z = cmplx(x, y, 8)
   if (z .ne. (4.0, 3.0)) call abort
   z = c
   if (z .ne. (3.0, 4.0)) call abort

   ! dcmplx intrinsic
   x = 3
   y = 4
   z = dcmplx (x, y)
   if (z .ne. (3.0, 4.0)) call abort

   ! conjucates and aimag
   c = (1.0, 2.0)
   c = conjg (c)
   x = aimag (c)
   if (abs (c - (1.0, -2.0)) .gt. 0.001) call abort
   if (x .ne. -2.0) call abort
   z = (2.0, 1.0)
   z = conjg (z)
   q = aimag (z)
   if (z .ne. (2.0, -1.0)) call abort
   if (q .ne. -1.0) call abort

   ! addition, subtraction and multiplication
   c = (1, 3)
   d = (5, 2)
   if (c + d .ne. ( 6, 5)) call abort
   if (c - d .ne. (-4, 1)) call abort
   if (c * d .ne. (-1, 17)) call abort

   ! test for constant folding
   if ((35.,-10.)**0.NE.(1.,0.)) call abort
end program
