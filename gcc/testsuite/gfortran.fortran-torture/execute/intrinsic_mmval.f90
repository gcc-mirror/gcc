! Program to test the MINVAL and MAXVAL intrinsics
program testmmval
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   logical, dimension (3, 3) :: m
   integer i

   a = reshape ((/1, 2, 3, 5, 4, 6, 9, 8, 7/), (/3, 3/));

   b = minval (a, 1)
   if (any(b .ne. (/1, 4, 7/))) call abort

   m = .true.
   m(1, 1) = .false.
   m(1, 2) = .false.
   b = minval (a, 1, m)
   if (any(b .ne. (/2, 4, 7/))) call abort

   b = maxval (a, 1)
   if (any(b .ne. (/3, 6, 9/))) call abort

   m = .true.
   m(1, 2) = .false.
   m(1, 3) = .false.
   b = maxval (a, 1, m)
   if (any(b .ne. (/3, 6, 8/))) call abort
end program
