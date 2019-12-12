! { dg-do compile }
! { dg-options "-fdec-static -fno-automatic -fdump-tree-original" }
!
! Neither of the local variable have the automatic attribute so they
! not be allocated on the stack.

subroutine foo
  integer :: a
  integer :: b
  equivalence (a, b)
  a = 5
  if (b.ne.5) stop 1
end subroutine

! { dg-final { scan-tree-dump "static union" "original" } }
! { dg-final { scan-tree-dump "static integer\\(kind=4\\) a" "original" } }
! { dg-final { scan-tree-dump "static integer\\(kind=4\\) b" "original" } }

