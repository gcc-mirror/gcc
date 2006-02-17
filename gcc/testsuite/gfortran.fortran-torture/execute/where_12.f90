! Check empty WHEREs work
program where_12
   integer :: a(5)

   a = (/1, 2, 3, 4, 5/)
   where (a .eq. 1)
   endwhere
end program

