program ptr
   implicit none
   integer, pointer, dimension(:) :: a, b
   integer, pointer :: p
   integer, target :: i

   allocate (a(1:6))
  
   a = (/ 1, 2, 3, 4, 5, 6 /)
   b => a
   if (any (b .ne. (/ 1, 2, 3, 4, 5, 6 /))) call abort
   b => a(1:6:2)
   if (any (b .ne. (/ 1, 3, 5/))) call abort
   
   p => i
   i = 42
   if (p .ne. 42) call abort
   p => a(4)
   if (p .ne. 4) call abort
end program
