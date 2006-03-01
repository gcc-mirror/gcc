! Check to ensure result is calculated from unmodified
! version of the right-hand-side in WHERE statements.
program where_19
   integer :: a(4)
   integer :: b(3)
   integer :: c(3)
   equivalence (a(1), b(1)), (a(2), c(1))

   a = (/1, 2, 3, 4/)
   where (b .gt. 1)
     c = b
   endwhere
   if (any (a .ne. (/1, 2, 2, 3/))) &
     call abort ()

   a = (/1, 2, 3, 4/)
   where (c .gt. 1)
     b = c
   endwhere
   if (any (a .ne. (/2, 3, 4, 4/))) &
     call abort ()
end program

