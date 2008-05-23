! { dg-do run }
! { dg-require-effective-target fortran_large_int }
! Program to test the eoshift intrinsic for kind=16_k integers
! 
program intrinsic_eoshift
  integer, parameter :: k=16
  integer(kind=k), dimension(3_k, 3_k) :: a
   integer(kind=k), dimension(3_k, 3_k, 2_k) :: b
   integer(kind=k), dimension(3_k) :: bo, sh

   ! Scalar shift and scalar bound.
   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, 1_k, 99_k, 1_k)
   if (any (a .ne. reshape ((/2_k, 3_k, 99_k, 5_k, 6_k, 99_k, 8_k, 9_k, 99_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, 9999_k, 99_k, 1_k)
   if (any (a .ne. 99_k)) call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, -2_k, dim = 2_k)
   if (any (a .ne. reshape ((/0_k, 0_k, 0_k, 0_k, 0_k, 0_k, 1_k, 2_k, 3_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, -9999_k, 99_k, 1_k)
   if (any (a .ne. 99_k)) call abort

   ! Array shift and scalar bound.
   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, (/1_k, 0_k, -1_k/), 99_k, 1_k)
   if (any (a .ne. reshape ((/2_k, 3_k, 99_k, 4_k, 5_k, 6_k, 99_k, 7_k, 8_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, (/9999_k, 0_k, -9999_k/), 99_k, 1_k)
   if (any (a .ne. reshape ((/99_k, 99_k, 99_k, 4_k, 5_k, 6_k, 99_k, 99_k, 99_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, (/2_k, -2_k, 0_k/), dim = 2_k)
   if (any (a .ne. reshape ((/7_k, 0_k, 3_k, 0_k, 0_k, 6_k, 0_k, 2_k, 9_k/), (/3_k, 3_k/)))) &
      call abort

   ! Scalar shift and array bound.
   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, 1_k, (/99_k, -1_k, 42_k/), 1_k)
   if (any (a .ne. reshape ((/2_k, 3_k, 99_k, 5_k, 6_k, -1_k, 8_k, 9_k, 42_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, 9999_k, (/99_k, -1_k, 42_k/), 1_k)
   if (any (a .ne. reshape ((/99_k, 99_k, 99_k, -1_k, -1_k, -1_k, 42_k, 42_k, 42_k/), &
       (/3_k, 3_k/)))) call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, -9999_k, (/99_k, -1_k, 42_k/), 1_k)
   if (any (a .ne. reshape ((/99_k, 99_k, 99_k, -1_k, -1_k, -1_k, 42_k, 42_k, 42_k/), &
       (/3_k, 3_k/)))) call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, -2_k, (/99_k, -1_k, 42_k/), 2_k)
   if (any (a .ne. reshape ((/99_k, -1_k, 42_k, 99_k, -1_k, 42_k, 1_k, 2_k, 3_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   bo = (/99_k, -1_k, 42_k/)
   a = eoshift (a, -2_k, bo, 2_k)
   if (any (a .ne. reshape ((/99_k, -1_k, 42_k, 99_k, -1_k, 42_k, 1_k, 2_k, 3_k/), (/3_k, 3_k/)))) &
      call abort

   ! Array shift and array bound.
   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, (/1_k, 0_k, -1_k/), (/99_k, -1_k, 42_k/), 1_k)
   if (any (a .ne. reshape ((/2_k, 3_k, 99_k, 4_k, 5_k, 6_k, 42_k, 7_k, 8_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, (/2_k, -2_k, 0_k/), (/99_k, -1_k, 42_k/), 2_k)
   if (any (a .ne. reshape ((/7_k, -1_k, 3_k, 99_k, -1_k, 6_k, 99_k, 2_k, 9_k/), (/3_k, 3_k/)))) &
      call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   sh = (/ 3_k, -1_k, -3_k /)
   bo = (/-999_k, -99_k, -9_k /)
   a = eoshift(a, shift=sh, boundary=bo)
   if (any (a .ne. reshape ((/ -999_k, -999_k, -999_k, -99_k, 4_k, 5_k, -9_k, -9_k, -9_k /), &
        shape(a)))) call abort

   a = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   a = eoshift (a, (/9999_k, -9999_k, 0_k/), (/99_k, -1_k, 42_k/), 2_k)
   if (any (a .ne. reshape ((/99_k, -1_k, 3_k, 99_k, -1_k, 6_k, 99_k, -1_k, 9_k/), (/3_k, 3_k/)))) &
      call abort

   ! Test arrays > rank 2
   b(:, :, 1_k) = reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   b(:, :, 2_k) = 10_k + reshape ((/1_k, 2_k, 3_k, 4_k, 5_k, 6_k, 7_k, 8_k, 9_k/), (/3_k, 3_k/))
   b = eoshift (b, 1_k, 99_k, 1_k)
   if (any (b(:, :, 1_k) .ne. reshape ((/2_k, 3_k, 99_k, 5_k, 6_k, 99_k, 8_k, 9_k, 99_k/), (/3_k, 3_k/)))) &
      call abort
   if (any (b(:, :, 2_k) .ne. reshape ((/12_k, 13_k, 99_k, 15_k, 16_k, 99_k, 18_k, 19_k, 99_k/), (/3_k, 3_k/)))) &
      call abort

   ! TODO: Test array sections
end program
