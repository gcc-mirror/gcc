! Program to test intrinsic bitops
program intrinsic_bitops
   implicit none
   integer(kind=4) :: i, j, k, o, t
   integer(kind=8) :: a, b, c

   o = 0
   i = 2
   j = 3
   k = 12

   if (.not. btest (i, o+1)) call abort
   if (btest (i, o+2)) call abort
   if (iand (i, j) .ne. 2) call abort
   if (ibclr (j, o+1) .ne. 1) call abort
   if (ibclr (j, o+2) .ne. 3) call abort
   if (ibits (k, o+1, o+2) .ne. 2) call abort
   if (ibset (j, o+1) .ne. 3) call abort
   if (ibset (j, o+2) .ne. 7) call abort
   if (ieor (i, j) .ne. 1) call abort
   if (ior (i, j) .ne. 3) call abort
   if (ishft (k, o+2) .ne. 48) call abort
   if (ishft (k, o-3) .ne. 1) call abort
   if (ishft (k, o) .ne. 12) call abort
   if (ishftc (k, o+30) .ne. 3) call abort
   if (ishftc (k, o-30) .ne. 48) call abort
   if (ishftc (k, o+1, o+3) .ne. 9) call abort
   if (not (i) .ne. -3) call abort
end program
