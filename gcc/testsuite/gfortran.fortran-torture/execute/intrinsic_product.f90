! Program to test the PRODUCT intrinsic
program testproduct
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   logical, dimension (3, 3) :: m, tr
   character(len=12) line

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/));

   b = product (a, 1)

   tr = .true.

   if (any(b .ne. (/6, 120, 504/))) call abort

   write (line, 9000) product(a,1)
   if (line .ne. '   6 120 504') call abort

   if (product (a) .ne. 362880) call abort

   write (line, 9010) product(a)
   if (line .ne. '362880') call abort

   m = .true.
   m(1, 1) = .false.
   m(2, 1) = .false.

   b = product (a, 2, m)
   if (any(b .ne. (/28, 40, 162/))) call abort

   b = product (a, 2, m .and. tr)
   if (any(b .ne. (/28, 40, 162/))) call abort

   write (line, 9000) product(a, 2, m)
   if (line .ne. '  28  40 162') call abort

   if (product (a, mask=m) .ne. 181440) call abort

   if (product (a, mask=m .and. tr) .ne. 181440) call abort

   write (line, 9010) product(a, mask=m)
   if (line .ne. '181440') call abort

9000 format (3I4)
9010 format (I6)
end program
