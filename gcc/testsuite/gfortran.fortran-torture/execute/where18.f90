! Check to ensure mask is calculated first in WHERE
! statements.
program where_18
   integer :: a(4)
   integer :: b(3)
   integer :: c(3)
   equivalence (a(1), b(1)), (a(2), c(1))

   a = (/1, 1, 1, 1/)
   where (b .eq. 1)
     c = 2
   elsewhere (b .eq. 2)
     c = 3
   endwhere
   if (any (a .ne. (/1, 2, 2, 2/))) &
     call abort

   a = (/1, 1, 1, 1/)
   where (c .eq. 1)
     b = 2
   elsewhere (b .eq. 2)
     b = 3
   endwhere
   if (any (a .ne. (/2, 2, 2, 1/))) &
     call abort
end program
