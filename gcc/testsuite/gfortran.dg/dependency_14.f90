! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a,i)
  integer, dimension (4,4) :: a
  integer :: i

  where (a(i,1:3) .ne. 0)
    a(i+1,2:4) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
