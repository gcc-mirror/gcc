! Program to test SIGN intrinsic
program intrinsic_sign
   implicit none
   integer i, j
   real r, s

   i = 2
   j = 3
   if (sign (i, j) .ne. 2) STOP 1
   i = 4
   j = -5
   if (sign (i, j) .ne. -4) STOP 2
   i = -6
   j = 7
   if (sign (i, j) .ne. 6) STOP 3
   i = -8
   j = -9
   if (sign (i, j) .ne. -8) STOP 4
   r = 1
   s = 2
   if (sign (r, s) .ne. 1) STOP 5
   r = 1
   s = -2
   if (sign (r, s) .ne. -1) STOP 6
   s = 0
   if (sign (r, s) .ne. 1) STOP 7
   ! Will fail on machines which cannot represent negative zero.
   s = -s ! Negative zero
   if (sign (r, s) .ne. -1) STOP 8
end program

