! Check whether an empty ELSEWHERE works
program where_14
   integer :: a(5)
   integer :: b(5)

   a = (/1, 2, 3, 4, 5/)
   b = (/0, 0, 0, 0, 0/)
   where (a .eq. 1)
     b = 3
   elsewhere
   endwhere
   if (any (b .ne. (/3, 0, 0, 0, 0/))) &
     STOP 1
end program

