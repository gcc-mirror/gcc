! Program to test the COUNT intrinsic
program intrinsic_count
   implicit none
   logical(kind=4), dimension (3, 5) :: a
   integer(kind=4), dimension (3) :: b
   integer i
   character(len=10) line

   a = .false.
   if (count(a) .ne. 0) call abort
   a = .true.
   if (count(a) .ne. 15) call abort
   a(1, 1) = .false.
   a(2, 2) = .false.
   a(2, 5) = .false.
   if (count(a) .ne. 12) call abort
   write (line, 9000) count(a)
   read (line, 9000) i
   if (i .ne. 12) call abort

   b(1:3) = count(a, 2);
   if (b(1) .ne. 4) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 5) call abort
   b = 0
   write (line, 9000) count(a,2)
   read (line, 9000) b
   if (b(1) .ne. 4) call abort
   if (b(2) .ne. 3) call abort
   if (b(3) .ne. 5) call abort

9000 format(3I3)

end program
