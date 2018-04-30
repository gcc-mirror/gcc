! Tests WHERE statement with a data dependency
program where_4
   integer, dimension(5) :: a
   integer, dimension(5) :: b

   a = (/1, 2, 3, 4, 5/)
   b = (/1, 0, 1, 0, 1/)
   
   where (b .ne. 0)
      a(:) = a(5:1:-1)
   endwhere
   if (any (a .ne. (/5, 2, 3, 4, 1/))) STOP 1
end program
