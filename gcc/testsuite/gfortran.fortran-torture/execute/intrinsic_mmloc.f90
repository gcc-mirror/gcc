! Program to test the MINLOC and MAXLOC intrinsics
program testmmloc
   implicit none
   integer, dimension (3, 3) :: a
   integer, dimension (3) :: b
   logical, dimension (3, 3) :: m, tr
   integer i
   character(len=10) line

   a = reshape ((/1, 2, 3, 5, 4, 6, 9, 8, 7/), (/3, 3/));
   tr = .true.

   b = minloc (a, 1)
   if (b(1) .ne. 1) call abort
   if (b(2) .ne. 2) call abort
   if (b(3) .ne. 3) call abort
   b = -1
   write (line, 9000) minloc(a,1)
   read (line, 9000) b
   if (b(1) .ne. 1) call abort
   if (b(2) .ne. 2) call abort
   if (b(3) .ne. 3) call abort

   m = .true.
   m(1, 1) = .false.
   m(1, 2) = .false.
   b = minloc (a, 1, m)
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 2) call abort
   if (b(3) .ne. 3) call abort
   b = minloc (a, 1, m .and. tr)
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 2) call abort
   if (b(3) .ne. 3) call abort
   b = -1
   write (line, 9000) minloc(a, 1, m)
   read (line, 9000) b
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 2) call abort
   if (b(3) .ne. 3) call abort

   b(1:2) = minloc(a)
   if (b(1) .ne. 1) call abort
   if (b(2) .ne. 1) call abort
   b = -1
   write (line, 9000) minloc(a)
   read (line, 9000) b
   if (b(1) .ne. 1) call abort
   if (b(2) .ne. 1) call abort
   if (b(3) .ne. 0) call abort

   b(1:2) = minloc(a, mask=m)
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 1) call abort
   b(1:2) = minloc(a, mask=m .and. tr)
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 1) call abort
   b = -1
   write (line, 9000) minloc(a, mask=m)
   read (line, 9000) b
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 1) call abort
   if (b(3) .ne. 0) call abort

   b = maxloc (a, 1)
   if (b(1) .ne. 3) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 1) call abort
   b = -1
   write (line, 9000) maxloc(a, 1)
   read (line, 9000) b
   if (b(1) .ne. 3) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 1) call abort

   m = .true.
   m(1, 2) = .false.
   m(1, 3) = .false.
   b = maxloc (a, 1, m)
   if (b(1) .ne. 3) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 2) call abort
   b = maxloc (a, 1, m .and. tr)
   if (b(1) .ne. 3) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 2) call abort
   b = -1
   write (line, 9000) maxloc(a, 1, m)
   read (line, 9000) b
   if (b(1) .ne. 3) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 2) call abort

   b(1:2) = maxloc(a)
   if (b(1) .ne. 1) call abort
   if (b(2) .ne. 3) call abort
   b = -1
   write (line, 9000) maxloc(a)
   read (line, 9000) b
   if (b(1) .ne. 1) call abort
   if (b(2) .ne. 3) call abort

   b(1:2) = maxloc(a, mask=m)
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 3) call abort
   b(1:2) = maxloc(a, mask=m .and. tr)
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 3) call abort
   b = -1
   write (line, 9000) maxloc(a, mask=m)
   read (line, 9000) b
   if (b(1) .ne. 2) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 0) call abort

9000 format (3I3)
end program
