! Program to test SIGN intrinsic
program intrinsic_sign
   implicit none
   integer i, j
   real r, s

   i = 2
   j = 3
   if (sign (i, j) .ne. 2) call abort
   i = 4
   j = -5
   if (sign (i, j) .ne. -4) call abort
   i = -6
   j = 7
   if (sign (i, j) .ne. 6) call abort
   i = -8
   j = -9
   if (sign (i, j) .ne. -8) call abort
   r = 1
   s = 2
   if (sign (r, s) .ne. 1) call abort
   r = 1
   s = -2
   if (sign (r, s) .ne. -1) call abort
   s = 0
   if (sign (r, s) .ne. 1) call abort
   ! Will fail on machines which cannot represent negative zero.
   s = -s ! Negative zero
   if (sign (r, s) .ne. -1) call abort
end program

