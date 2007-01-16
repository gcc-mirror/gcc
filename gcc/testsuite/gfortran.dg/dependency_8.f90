! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a,i,j)
  integer, dimension (4,4) :: a
  integer :: i
  integer :: j

  where (a(i,1:3) .ne. 0)
    a(j,2:4) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "temp" 3 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
