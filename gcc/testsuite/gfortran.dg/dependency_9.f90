! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a,i,j)
  integer, dimension (4,4) :: a
  integer :: i
  integer :: j

  where (a(i,:) .ne. 0)
    a(j,:) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
