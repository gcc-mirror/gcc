! Program to test min and max intrinsics
program intrinsic_minmax
   implicit none
   integer i, j, k, m
   real r, s, t, u

   i = 1
   j = -2
   k = 3
   m = 4
   if (min (i, k) .ne. 1) call abort
   if (min (i, j, k, m) .ne. -2) call abort
   if (max (i, k) .ne. 3) call abort
   if (max (i, j, k, m) .ne. 4) call abort
   if (max (i+1, j) .ne. 2) call abort

   r = 1
   s = -2
   t = 3
   u = 4
   if (min (r, t) .ne. 1) call abort
   if (min (r, s, t, u) .ne. -2) call abort
   if (max (r, t) .ne. 3) call abort
   if (max (r, s, t, u) .ne. 4) call abort

   if (max (4d0, r) .ne. 4d0) call abort
   if (amax0 (i, j) .ne. 1.0) call abort
   if (min1 (r, s) .ne. -2) call abort

   ! Test simplify.
   if (min (1, -2, 3, 4) .ne. -2) call abort
   if (max (1, -2, 3, 4) .ne. 4) call abort
   if (amax0 (1, -2) .ne. 1.0) call abort
   if (min1 (1., -2.) .ne. -2) call abort

end program

