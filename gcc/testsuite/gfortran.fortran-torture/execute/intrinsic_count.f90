! Program to test the COUNT intrinsic
program intrinsic_count
   implicit none
   logical(kind=4), dimension (3, 5) :: a
   integer(kind=4), dimension (3) :: b
   integer i
   character(len=10) line

   a = .false.
   if (count(a) .ne. 0) STOP 1
   a = .true.
   if (count(a) .ne. 15) STOP 2
   a(1, 1) = .false.
   a(2, 2) = .false.
   a(2, 5) = .false.
   if (count(a) .ne. 12) STOP 3
   write (line, 9000) count(a)
   read (line, 9000) i
   if (i .ne. 12) STOP 4

   b(1:3) = count(a, 2);
   if (b(1) .ne. 4) STOP 5
   if (b(2) .ne. 3) STOP 6
   if (b(3) .ne. 5) STOP 7
   b = 0
   write (line, 9000) count(a,2)
   read (line, 9000) b
   if (b(1) .ne. 4) STOP 8
   if (b(2) .ne. 3) STOP 9
   if (b(3) .ne. 5) STOP 10

9000 format(3I3)

end program
