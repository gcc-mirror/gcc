! { dg-do compile }
! { dg-options "-O2 -fdump-ipa-cp-details -fno-inline -fdump-tree-optimized" }

subroutine bar (a, b, n)
  integer :: a(n), b(n)
  call foo (a, b)
contains
subroutine foo (a, b)
  integer :: a(:), b(:)
  a = b
end subroutine
end

! { dg-final { scan-ipa-dump "Creating a specialized node of foo" "cp" } }
! { dg-final { scan-ipa-dump-times "Aggregate replacements\[^=\]*=\[^=\]*=\[^=\]*=\[^=\]*=\[^=\]*=\[^=\]*=\[^=\]*=\[^=\]*=" 2 "cp" } }
! { dg-final { scan-tree-dump-not "stride;" "optimized" } }
! { dg-final { scan-tree-dump-not "lbound;" "optimized" } }
