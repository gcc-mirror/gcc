! Program to test the ABS intrinsic
program intrinsic_abs
   implicit none
   integer i
   real(kind=4) r
   real(kind=8) q
   complex z

   i = 42
   i = abs(i)
   if (i .ne. 42) call abort
   i = -43
   i = abs(i)
   if (i .ne. 43) call abort

   r = 42.0
   r = abs(r)
   if (r .ne. 42.0) call abort
   r = -43.0
   r = abs(r)
   if (r .ne. 43.0) call abort

   q = 42.0_8
   q = abs(q)
   if (q .ne. 42.0_8) call abort
   q = -43.0_8
   q = abs(q)
   if (q .ne. 43.0_8) call abort

   z = (3, 4)
   r = abs(z)
   if (r .ne. 5) call abort
end program
