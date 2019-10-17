! { dg-do compile }
! { dg-options "-fdec-static -fdump-tree-original" }
!

subroutine foo
  integer :: a
  integer, automatic :: b
  equivalence (a, b)
  a = 5
  if (b.ne.5) stop 1
end subroutine

! { dg-final { scan-tree-dump "union" "original" } }
! { dg-final { scan-tree-dump-not "static union" "original" } }
! { dg-final { scan-tree-dump "integer\\(kind=4\\) a" "original" } }
! { dg-final { scan-tree-dump-not "static integer\\(kind=4\\) a" "original" } }
! { dg-final { scan-tree-dump "integer\\(kind=4\\) b" "original" } }
! { dg-final { scan-tree-dump-not "static integer\\(kind=4\\) b" "original" } }

