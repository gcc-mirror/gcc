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
   if (any (b .ne. (/6, 5/))) call abort
   if (a(1, 1) .ne. 42) call abort
   if (a(6, 5) .ne. 43) call abort
end subroutine
end program

