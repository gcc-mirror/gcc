! Tests WHERE satement with non-integer array in the mask expression
program where_5
   integer, dimension(5) :: a
   real(kind=8), dimension(5) :: b

   a = (/1, 2, 3, 4, 5/)
   b = (/1d0, 0d0, 1d0, 0d0, 1d0/)
   
   where (b .ne. 0d0)
      a(:) = a(:) + 10
   endwhere
   if (any (a .ne. (/11, 2, 13, 4, 15/))) STOP 1
end program
