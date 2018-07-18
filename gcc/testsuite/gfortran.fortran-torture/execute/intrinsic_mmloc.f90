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
   if (b(1) .ne. 1) STOP 1
   if (b(2) .ne. 2) STOP 2
   if (b(3) .ne. 3) STOP 3
   b = -1
   write (line, 9000) minloc(a,1)
   read (line, 9000) b
   if (b(1) .ne. 1) STOP 4
   if (b(2) .ne. 2) STOP 5
   if (b(3) .ne. 3) STOP 6

   m = .true.
   m(1, 1) = .false.
   m(1, 2) = .false.
   b = minloc (a, 1, m)
   if (b(1) .ne. 2) STOP 7
   if (b(2) .ne. 2) STOP 8
   if (b(3) .ne. 3) STOP 9
   b = minloc (a, 1, m .and. tr)
   if (b(1) .ne. 2) STOP 10
   if (b(2) .ne. 2) STOP 11
   if (b(3) .ne. 3) STOP 12
   b = -1
   write (line, 9000) minloc(a, 1, m)
   read (line, 9000) b
   if (b(1) .ne. 2) STOP 13
   if (b(2) .ne. 2) STOP 14
   if (b(3) .ne. 3) STOP 15

   b(1:2) = minloc(a)
   if (b(1) .ne. 1) STOP 16
   if (b(2) .ne. 1) STOP 17
   b = -1
   write (line, 9000) minloc(a)
   read (line, 9000) b
   if (b(1) .ne. 1) STOP 18
   if (b(2) .ne. 1) STOP 19
   if (b(3) .ne. 0) STOP 20

   b(1:2) = minloc(a, mask=m)
   if (b(1) .ne. 2) STOP 21
   if (b(2) .ne. 1) STOP 22
   b(1:2) = minloc(a, mask=m .and. tr)
   if (b(1) .ne. 2) STOP 23
   if (b(2) .ne. 1) STOP 24
   b = -1
   write (line, 9000) minloc(a, mask=m)
   read (line, 9000) b
   if (b(1) .ne. 2) STOP 25
   if (b(2) .ne. 1) STOP 26
   if (b(3) .ne. 0) STOP 27

   b = maxloc (a, 1)
   if (b(1) .ne. 3) STOP 28
   if (b(2) .ne. 3) STOP 29
   if (b(3) .ne. 1) STOP 30
   b = -1
   write (line, 9000) maxloc(a, 1)
   read (line, 9000) b
   if (b(1) .ne. 3) STOP 31
   if (b(2) .ne. 3) STOP 32
   if (b(3) .ne. 1) STOP 33

   m = .true.
   m(1, 2) = .false.
   m(1, 3) = .false.
   b = maxloc (a, 1, m)
   if (b(1) .ne. 3) STOP 34
   if (b(2) .ne. 3) STOP 35
   if (b(3) .ne. 2) STOP 36
   b = maxloc (a, 1, m .and. tr)
   if (b(1) .ne. 3) STOP 37
   if (b(2) .ne. 3) STOP 38
   if (b(3) .ne. 2) STOP 39
   b = -1
   write (line, 9000) maxloc(a, 1, m)
   read (line, 9000) b
   if (b(1) .ne. 3) STOP 40
   if (b(2) .ne. 3) STOP 41
   if (b(3) .ne. 2) STOP 42

   b(1:2) = maxloc(a)
   if (b(1) .ne. 1) STOP 43
   if (b(2) .ne. 3) STOP 44
   b = -1
   write (line, 9000) maxloc(a)
   read (line, 9000) b
   if (b(1) .ne. 1) STOP 45
   if (b(2) .ne. 3) STOP 46

   b(1:2) = maxloc(a, mask=m)
   if (b(1) .ne. 2) STOP 47
   if (b(2) .ne. 3) STOP 48
   b(1:2) = maxloc(a, mask=m .and. tr)
   if (b(1) .ne. 2) STOP 49
   if (b(2) .ne. 3) STOP 50
   b = -1
   write (line, 9000) maxloc(a, mask=m)
   read (line, 9000) b
   if (b(1) .ne. 2) STOP 51
   if (b(2) .ne. 3) STOP 52
   if (b(3) .ne. 0) STOP 53

9000 format (3I3)
end program
