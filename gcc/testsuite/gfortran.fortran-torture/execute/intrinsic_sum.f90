! Program to test the FORALL construct
program testforall
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   logical, dimension (3, 3) :: m, tr
   integer i
   character(len=9) line

   a = reshape ((/1, 2, 3, 4, 5, 6, 7, 8, 9/), (/3, 3/));

   tr = .true.

   if (sum(a) .ne. 45) call abort
   write (line, 9000) sum(a)
   if (line .ne. ' 45      ') call abort
   b = sum (a, 1)
   if (b(1) .ne. 6) call abort
   if (b(2) .ne. 15) call abort
   if (b(3) .ne. 24) call abort
   write (line, 9000) sum (a, 1)
   if (line .ne. '  6 15 24') call abort

   m = .true.
   m(1, 1) = .false.
   m(2, 1) = .false.

   if (sum (a, mask=m) .ne. 42) call abort
   if (sum (a, mask=m .and. tr) .ne. 42) call abort

   write(line, 9000) sum (a, mask=m)
   if (line .ne. ' 42      ') call abort

   b = sum (a, 2, m)
   if (b(1) .ne. 11) call abort
   if (b(2) .ne. 13) call abort
   if (b(3) .ne. 18) call abort

   b = sum (a, 2, m .and. tr)
   if (b(1) .ne. 11) call abort
   if (b(2) .ne. 13) call abort
   if (b(3) .ne. 18) call abort
   write (line, 9000) sum (a, 2, m)
   if (line .ne. ' 11 13 18') call abort

9000 format(3I3)
end program
