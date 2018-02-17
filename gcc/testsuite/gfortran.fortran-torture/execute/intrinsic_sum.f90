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

   if (sum(a) .ne. 45) STOP 1
   write (line, 9000) sum(a)
   if (line .ne. ' 45      ') STOP 2
   b = sum (a, 1)
   if (b(1) .ne. 6) STOP 3
   if (b(2) .ne. 15) STOP 4
   if (b(3) .ne. 24) STOP 5
   write (line, 9000) sum (a, 1)
   if (line .ne. '  6 15 24') STOP 6

   m = .true.
   m(1, 1) = .false.
   m(2, 1) = .false.

   if (sum (a, mask=m) .ne. 42) STOP 7
   if (sum (a, mask=m .and. tr) .ne. 42) STOP 8

   write(line, 9000) sum (a, mask=m)
   if (line .ne. ' 42      ') STOP 9

   b = sum (a, 2, m)
   if (b(1) .ne. 11) STOP 10
   if (b(2) .ne. 13) STOP 11
   if (b(3) .ne. 18) STOP 12

   b = sum (a, 2, m .and. tr)
   if (b(1) .ne. 11) STOP 13
   if (b(2) .ne. 13) STOP 14
   if (b(3) .ne. 18) STOP 15
   write (line, 9000) sum (a, 2, m)
   if (line .ne. ' 11 13 18') STOP 16

9000 format(3I3)
end program
