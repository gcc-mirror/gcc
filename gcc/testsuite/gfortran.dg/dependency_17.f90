! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a,i)
  integer, dimension (3,3,4) :: a
  integer :: i

  where (a(1,1:2,1:3) .ne. 0)
    a(2:3,3,2:4) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
