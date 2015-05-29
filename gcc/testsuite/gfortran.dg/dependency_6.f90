! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine foo(a)
  integer, dimension (4) :: a

  where (a(:4) .ne. 0)
    a(:4) = 1
  endwhere
end subroutine
! { dg-final { scan-tree-dump-times "malloc" 0 "original" } }
