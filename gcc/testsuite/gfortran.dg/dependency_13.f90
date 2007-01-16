! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
   integer :: i(5)
   real(4) :: x(5)
   equivalence(x,i)

   i = (/ 1, 0, 3, 5, 0 /)
   where (i(1:4) .ne. 0)
     x(2:5) = -42.
   end where
   end
! { dg-final { scan-tree-dump-times "temp" 3 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
