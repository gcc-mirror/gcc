! Program to test the eoshift intrinsic
program intrinsic_eoshift
   integer, dimension(3, 3) :: a
   integer, dimension(3, 3, 2) :: b
   integer, dimension(3) :: bo, sh

   ! Scalar shift and scalar bound.
   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, 1, 99, 1)
   if (any (a .ne. reshape ((/2, 3, 99, 5, 6, 99, 8, 9, 99/), (/3, 3/)))) &
      STOP 1

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, 9999, 99, 1)
   if (any (a .ne. 99)) STOP 2

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, -2, dim = 2)
   if (any (a .ne. reshape ((/0, 0, 0, 0, 0, 0, 1, 2, 3/), (/3, 3/)))) &
      STOP 3

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, -9999, 99, 1)
   if (any (a .ne. 99)) STOP 4

   ! Array shift and scalar bound.
   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, (/1, 0, -1/), 99, 1)
   if (any (a .ne. reshape ((/2, 3, 99, 4, 5, 6, 99, 7, 8/), (/3, 3/)))) &
      STOP 5

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, (/9999, 0, -9999/), 99, 1)
   if (any (a .ne. reshape ((/99, 99, 99, 4, 5, 6, 99, 99, 99/), (/3, 3/)))) &
      STOP 6

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, (/2, -2, 0/), dim = 2)
   if (any (a .ne. reshape ((/7, 0, 3, 0, 0, 6, 0, 2, 9/), (/3, 3/)))) &
      STOP 7

   ! Scalar shift and array bound.
   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, 1, (/99, -1, 42/), 1)
   if (any (a .ne. reshape ((/2, 3, 99, 5, 6, -1, 8, 9, 42/), (/3, 3/)))) &
      STOP 8

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, 9999, (/99, -1, 42/), 1)
   if (any (a .ne. reshape ((/99, 99, 99, -1, -1, -1, 42, 42, 42/), &
	(/3, 3/)))) STOP 9

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, -9999, (/99, -1, 42/), 1)
   if (any (a .ne. reshape ((/99, 99, 99, -1, -1, -1, 42, 42, 42/), &
	(/3, 3/)))) STOP 10

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, -2, (/99, -1, 42/), 2)
   if (any (a .ne. reshape ((/99, -1, 42, 99, -1, 42, 1, 2, 3/), (/3, 3/)))) &
      STOP 11

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   bo = (/99, -1, 42/)
   a = eoshift (a, -2, bo, 2)
   if (any (a .ne. reshape ((/99, -1, 42, 99, -1, 42, 1, 2, 3/), (/3, 3/)))) &
      STOP 12

   ! Array shift and array bound.
   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, (/1, 0, -1/), (/99, -1, 42/), 1)
   if (any (a .ne. reshape ((/2, 3, 99, 4, 5, 6, 42, 7, 8/), (/3, 3/)))) &
      STOP 13

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, (/2, -2, 0/), (/99, -1, 42/), 2)
   if (any (a .ne. reshape ((/7, -1, 3, 99, -1, 6, 99, 2, 9/), (/3, 3/)))) &
      STOP 14

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   sh = (/ 3, -1, -3 /)
   bo = (/-999, -99, -9 /)
   a = eoshift(a, shift=sh, boundary=bo)
   if (any (a .ne. reshape ((/ -999, -999, -999, -99, 4, 5, -9, -9, -9 /), &
        shape(a)))) STOP 15

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   a = eoshift (a, (/9999, -9999, 0/), (/99, -1, 42/), 2)
   if (any (a .ne. reshape ((/99, -1, 3, 99, -1, 6, 99, -1, 9/), (/3, 3/)))) &
      STOP 16

   ! Test arrays > rank 2
   b(:, :, 1) = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   b(:, :, 2) = 10 + reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/))
   b = eoshift (b, 1, 99, 1)
   if (any (b(:, :, 1) .ne. reshape ((/2, 3, 99, 5, 6, 99, 8, 9, 99/), (/3, 3/)))) &
      STOP 17
   if (any (b(:, :, 2) .ne. reshape ((/12, 13, 99, 15, 16, 99, 18, 19, 99/), (/3, 3/)))) &
      STOP 18

   ! TODO: Test array sections
end program
