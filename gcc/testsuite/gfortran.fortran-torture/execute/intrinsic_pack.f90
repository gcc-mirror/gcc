! Program to test the PACK intrinsic
program intrinsic_pack
   integer, dimension(3, 3) :: a
   integer, dimension(6) :: b

   a = reshape ((/0, 0, 0, 0, 9, 0, 0, 0, 7/), (/3, 3/))
   b = 0
   b(1:6:3) = pack (a, a .ne. 0);
   if (any (b(1:6:3) .ne. (/9, 7/))) call abort
   b = pack (a(2:3, 2:3), a(2:3, 2:3) .ne. 0, (/1, 2, 3, 4, 5, 6/));
   if (any (b .ne. (/9, 7, 3, 4, 5, 6/))) call abort
end program
