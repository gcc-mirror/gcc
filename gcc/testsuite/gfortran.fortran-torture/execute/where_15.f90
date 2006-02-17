! Check whether an empty WHERE works
program where_15
   integer :: a(5)
   integer :: b(5)

   a = (/1, 2, 3, 4, 5/)
   b = (/0, 0, 0, 0, 0/)
   where (a .eq. 1)
   elsewhere
     b = 2
   endwhere
   if (any (b .ne. (/0, 2, 2, 2, 2/))) &
     call abort
end program

