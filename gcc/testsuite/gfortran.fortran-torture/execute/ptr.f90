program ptr
   implicit none
   integer, pointer, dimension(:) :: a, b
   integer, pointer :: p
   integer, target :: i

   allocate (a(1:6))
  
   a = (/ 1, 2, 3, 4, 5, 6 /)
   b => a
   if (any (b .ne. (/ 1, 2, 3, 4, 5, 6 /))) STOP 1
   b => a(1:6:2)
   if (any (b .ne. (/ 1, 3, 5/))) STOP 2
   
   p => i
   i = 42
   if (p .ne. 42) STOP 3
   p => a(4)
   if (p .ne. 4) STOP 4
end program
