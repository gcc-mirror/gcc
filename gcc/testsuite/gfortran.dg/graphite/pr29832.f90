! { dg-do run }
! { dg-options "-O2 -ftree-loop-linear" }

! Program to test the scalarizer
program testarray
   implicit none
   integer, dimension (6, 5) :: a, b
   integer n

   a = 0
   do n = 1, 5
      a(4, n) = n
   end do

   b(:, 5:1:-1) = a
   a(1:5, 2) = a(4, :) + 1

   ! The following expression should cause loop reordering
   a(:, 2:4) = a(:, 1:3)

   do n = 1, 5
      if (a(n, 3) .ne. (n + 1)) call abort
      if (b(4, n) .ne. (6 - n)) call abort
   end do
end program

