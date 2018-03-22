! Check whether nested WHEREs work
program where_16
   integer :: a(9)
   integer :: b(9)
   integer :: c(9)

   a = (/0, 0, 0, 1, 1, 1, 2, 2, 2/)
   b = (/0, 1, 2, 0, 1, 2, 0, 1, 2/)
   c = (/0, 0, 0, 0, 0, 0, 0, 0, 0/)

   where (a .eq. 0)
     where (b .eq. 0)
       c = 1
     else where (b .eq. 1)
       c = 2
     else where
       c = 3
     endwhere
   elsewhere (a .eq. 1)
     where (b .eq. 0)
       c = 4
     else where (b .eq. 1)
       c = 5
     else where
       c = 6
     endwhere
   elsewhere
     where (b .eq. 0)
       c = 7
     else where (b .eq. 1)
       c = 8
     else where
       c = 9
     endwhere
   endwhere
   if (any (c .ne. (/1, 2, 3, 4, 5, 6, 7, 8, 9/))) &
     STOP 1
end program

