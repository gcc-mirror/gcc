! Program to test the ANY and ALL intrinsics
program anyall
   implicit none
   logical, dimension(3, 3) :: a
   logical, dimension(3) :: b
   character(len=10) line

   a = .false.
   if (any(a)) STOP 1
   a(1, 1) = .true.
   a(2, 3) = .true.
   if (.not. any(a)) STOP 2
   b = any(a, 1)
   if (.not. b(1)) STOP 3
   if (b(2)) STOP 4
   if (.not. b(3)) STOP 5
   b = .false.
   write (line, 9000) any(a,1)
   read (line, 9000) b
   if (.not. b(1)) STOP 6
   if (b(2)) STOP 7
   if (.not. b(3)) STOP 8

   a = .true.
   if (.not. all(a)) STOP 9
   a(1, 1) = .false.
   a(2, 3) = .false.
   if (all(a)) STOP 10
   b = all(a, 1)
   if (b(1)) STOP 11
   if (.not. b(2)) STOP 12
   if (b(3)) STOP 13
   b = .false.
   write (line, 9000) all(a,1)
   read (line, 9000) b
   if (b(1)) STOP 14
   if (.not. b(2)) STOP 15
   if (b(3)) STOP 16

9000 format (9L1)
end program
