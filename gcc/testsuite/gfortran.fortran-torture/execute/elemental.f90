! Program to test elemental functions.
program test_elemental
   implicit none
   integer, dimension (2, 4) :: a
   integer, dimension (2, 4) :: b
   integer(kind = 8), dimension(2) :: c

   a = reshape ((/2, 3, 4, 5, 6, 7, 8, 9/), (/2, 4/))
   b = 0
   b(2, :) = e_fn (a(1, :), 1)
   if (any (b .ne. reshape ((/0, 1, 0, 3, 0, 5, 0, 7/), (/2, 4/)))) call abort
   a = e_fn (a(:, 4:1:-1), 1 + b)
   if (any (a .ne. reshape ((/7, 7, 5, 3, 3, -1, 1, -5/), (/2, 4/)))) call abort
   ! This tests intrinsic elemental conversion functions.
   c = 2 * a(1, 1)
   if (any (c .ne. 14)) call abort

   ! This triggered bug due to building ss chains in the wrong order.
   b = 0;
   a = a - e_fn (a, b)
   if (any (a .ne. 0)) call abort

   ! Check expressions involving constants
   a = e_fn (b + 1, 1)
   if (any (a .ne. 0)) call abort
contains

elemental integer(kind=4) function e_fn (p, q)
   integer, intent(in) :: p, q
   e_fn = p - q
end function
end program
