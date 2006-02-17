! Check empty WHERE and empty ELSEWHERE works
program where_13
   integer :: a(5)

   a = (/1, 2, 3, 4, 5/)
   where (a .eq. 2)
   elsewhere
   endwhere
end program

