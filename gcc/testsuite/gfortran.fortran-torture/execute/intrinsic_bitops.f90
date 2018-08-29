! Program to test intrinsic bitops
program intrinsic_bitops
   implicit none
   integer(kind=4) :: i, j, k, o, t
   integer(kind=8) :: a, b, c

   o = 0
   i = 2
   j = 3
   k = 12
   a = 5
   
   if (.not. btest (i, o+1)) STOP 1
   if (btest (i, o+2)) STOP 2
   if (iand (i, j) .ne. 2) STOP 3
   if (ibclr (j, o+1) .ne. 1) STOP 4
   if (ibclr (j, o+2) .ne. 3) STOP 5
   if (ibits (k, o+1, o+2) .ne. 2) STOP 6
   if (ibset (j, o+1) .ne. 3) STOP 7
   if (ibset (j, o+2) .ne. 7) STOP 8
   if (ieor (i, j) .ne. 1) STOP 9
   if (ior (i, j) .ne. 3) STOP 10
   if (ishft (k, o+2) .ne. 48) STOP 11
   if (ishft (k, o-3) .ne. 1) STOP 12
   if (ishft (k, o) .ne. 12) STOP 13
   if (ishftc (k, o+30) .ne. 3) STOP 14
   if (ishftc (k, o-30) .ne. 48) STOP 15
   if (ishftc (k, o+1, o+3) .ne. 9) STOP 16
   if (not (i) .ne. -3) STOP 17
   if (ishftc (a, 1, bit_size(a)) .ne. 10) STOP 18
   if (ishftc (1, 1, 32) .ne. 2) STOP 19
end program
