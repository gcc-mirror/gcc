! Program to test min and max intrinsics
program intrinsic_minmax
   implicit none
   integer i, j, k, m
   real r, s, t, u

   i = 1
   j = -2
   k = 3
   m = 4
   if (min (i, k) .ne. 1) STOP 1
   if (min (i, j, k, m) .ne. -2) STOP 2
   if (max (i, k) .ne. 3) STOP 3
   if (max (i, j, k, m) .ne. 4) STOP 4
   if (max (i+1, j) .ne. 2) STOP 5

   r = 1
   s = -2
   t = 3
   u = 4
   if (min (r, t) .ne. 1) STOP 6
   if (min (r, s, t, u) .ne. -2) STOP 7
   if (max (r, t) .ne. 3) STOP 8
   if (max (r, s, t, u) .ne. 4) STOP 9

   if (max (4d0, r) .ne. 4d0) STOP 10
   if (amax0 (i, j) .ne. 1.0) STOP 11
   if (min1 (r, s) .ne. -2) STOP 12

   ! Test simplify.
   if (min (1, -2, 3, 4) .ne. -2) STOP 13
   if (max (1, -2, 3, 4) .ne. 4) STOP 14
   if (amax0 (1, -2) .ne. 1.0) STOP 15
   if (min1 (1., -2.) .ne. -2) STOP 16

end program

