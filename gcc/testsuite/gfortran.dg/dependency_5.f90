! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a)
  integer, dimension (4) :: a

  where (a(:) .ne. 0)
    a(:) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
