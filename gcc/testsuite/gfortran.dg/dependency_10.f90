! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a)
  integer, dimension (4) :: a
  integer :: n

  n = 3
  where (a(:n) .ne. 0)
    a(:n) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
