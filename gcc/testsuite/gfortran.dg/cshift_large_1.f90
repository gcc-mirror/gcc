! { dg-do run }
! { dg-require-effective-target fortran_large_int }
! Program to test the cshift intrinsic for kind=16 integers
program intrinsic_cshift
   integer, parameter :: k=16
   integer(kind=k), dimension(3_k, 3_k) :: a
   integer(kind=k), dimension(3_k, 3_k, 2_k) :: b

   ! Scalar shift
   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = cshift (a, 1_k, 1_k)
   if (any (a .ne. reshape ((/2_k, 3_k, 1_k, 5_k, 6_k, 4_k, 8_k, 9_k, 7_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = cshift (a, -2_k, dim = 2_k)
   if (any (a .ne. reshape ((/4_k, 5_k, 6_k, 7_k, 8_k, 9_k, 1_k, 2_k, 3_k/), (/3_k, 3_k/)))) &
      call abort

   ! Array shift
   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = cshift (a, (/1_k, 0_k, -1_k/))
   if (any (a .ne. reshape ((/2_k, 3_k, 1_k, 4_k, 5_k, 6_k, 9_k, 7_k, 8_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = cshift (a, (/2_k, -2_k, 0_k/), dim = 2_k)
   if (any (a .ne. reshape ((/7_k, 5_k, 3_k, 1_k, 8_k, 6_k, 4_k, 2_k, 9_k/), (/3_k, 3_k/)))) &
      call abort

   ! Test arrays > rank 2
   b = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k, 11_k, 12_k, 13_k, 14_k, 15_k, 16_k, 17_k,&
         18_k, 19_k/), (/3_k, 3_k, 2_k/))
   b = cshift (b, 1_k)
   if (any (b .ne. reshape ((/2_k, 3_k, 1_k, 5_k, 6_k, 4_k, 8_k, 9_k, 7_k, 12_k, 13_k, 11_k, 15_k,&
     16_k, 14_k, 18_k, 19_k, 17_k/), (/3_k, 3_k, 2_k/)))) &
      call abort

   b = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k, 11_k, 12_k, 13_k, 14_k, 15_k, 16_k, 17_k,&
         18_k, 19_k/), (/3_k, 3_k, 2_k/))
   b = cshift (b, reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/)), 3_k)
   if (any (b .ne. reshape ((/11_k, 2_k, 13_k, 4_k, 15_k, 6_k, 17_k, 8_k, 19_k, 1_k, 12_k, 3_k,&
     14_k, 5_k, 16_k, 7_k, 18_k, 9_k/), (/3_k, 3_k, 2_k/)))) &
      call abort

end program
