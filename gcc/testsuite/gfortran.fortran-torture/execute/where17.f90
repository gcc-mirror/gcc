! Check to ensure only the first true clause in WHERE is
! executed.
program where_17
   integer :: a(3)

   a = (/1, 2, 3/)
   where (a .eq. 1)
     a = 2
   elsewhere (a .le. 2)
     a = 3
   elsewhere (a .le. 3)
     a = 4
   endwhere
   if (any (a .ne. (/2, 3, 4/))) STOP 1
end program
