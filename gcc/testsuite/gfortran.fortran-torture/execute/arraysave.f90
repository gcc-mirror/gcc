! Program to test arrays with the save attribute
program testarray
   implicit none
   integer, save, dimension (6, 5) :: a, b

   a = 0
   a(1, 1) = 42
   a(6, 5) = 43
   b(:,1:5) = a

   call fn (a)
contains
subroutine fn (a)
   implicit none
   integer, dimension(1:, 1:) :: a
   integer, dimension(2) :: b
   
   b = ubound (a)
   if (any (b .ne. (/6, 5/))) STOP 1
   if (a(1, 1) .ne. 42) STOP 2
   if (a(6, 5) .ne. 43) STOP 3
end subroutine
end program

