! Program to test the cshift intrinsic
program intrinsic_cshift
   integer, dimension(3, 3) :: a
   integer, dimension(3, 3, 2) :: b

   ! Scalar shift
   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = cshift (a, 1, 1)
   if (any (a .ne. reshape ((/2, 3, 1, 5, 6, 4, 8, 9, 7/), (/3, 3/)))) &
      call abort

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = cshift (a, -2, dim = 2)
   if (any (a .ne. reshape ((/4, 5, 6, 7, 8, 9, 1, 2, 3/), (/3, 3/)))) &
      call abort

   ! Array shift
   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = cshift (a, (/1, 0, -1/))
   if (any (a .ne. reshape ((/2, 3, 1, 4, 5, 6, 9, 7, 8/), (/3, 3/)))) &
      call abort

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = cshift (a, (/2, -2, 0/), dim = 2)
   if (any (a .ne. reshape ((/7, 5, 3, 1, 8, 6, 4, 2, 9/), (/3, 3/)))) &
      call abort

   ! Test arrays > rank 2
   b = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17,&
         18, 19/), (/3, 3, 2/))
   b = cshift (b, 1)
   if (any (b .ne. reshape ((/2, 3, 1, 5, 6, 4, 8, 9, 7, 12, 13, 11, 15,&
     16, 14, 18, 19, 17/), (/3, 3, 2/)))) &
      call abort

   b = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17,&
         18, 19/), (/3, 3, 2/))
   b = cshift (b, reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/)), 3)
   if (any (b .ne. reshape ((/11, 2, 13, 4, 15, 6, 17, 8, 19, 1, 12, 3,&
     14, 5, 16, 7, 18, 9/), (/3, 3, 2/)))) &
      call abort

end program
