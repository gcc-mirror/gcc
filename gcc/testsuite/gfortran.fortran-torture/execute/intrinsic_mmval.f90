! Program to test the MINVAL and MAXVAL intrinsics
program testmmval
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   logical, dimension (3, 3) :: m, tr
   integer i
   character (len=9) line

   a = reshape ((/1, 2, 3, 5, 4, 6, 9, 8, 7/), (/3, 3/));

   tr = .true.

   b = minval (a, 1)
   if (any(b .ne. (/1, 4, 7/))) call abort
   write (line, 9000) minval (a, 1)
   if (line .ne. '  1  4  7') call abort

   m = .true.
   m(1, 1) = .false.
   m(1, 2) = .false.
   b = minval (a, 1, m)
   if (any(b .ne. (/2, 4, 7/))) call abort
   b = minval (a, 1, m .and. tr)
   if (any(b .ne. (/2, 4, 7/))) call abort
   write (line, 9000) minval(a, 1, m)
   if (line .ne. '  2  4  7') call abort

   b = maxval (a, 1)
   if (any(b .ne. (/3, 6, 9/))) call abort
   write (line, 9000) maxval (a, 1)
   if (line .ne. '  3  6  9') call abort

   m = .true.
   m(1, 2) = .false.
   m(1, 3) = .false.
   b = maxval (a, 1, m)
   if (any(b .ne. (/3, 6, 8/))) call abort
   b = maxval (a, 1, m .and. tr)
   if (any(b .ne. (/3, 6, 8/))) call abort
   write (line, 9000) maxval(a, 1, m)
   if (line .ne. '  3  6  8') call abort

9000 format(3I3)
end program
