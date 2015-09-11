! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-optimized" }

function test(b)
  real a
  a = (b + 5.) - 5.
  test = a
end

! We need an explicit +5 and -5, and an intermediate ((bla)) expression
! (the reassoc barrier).  Make use of "." matching lineends.
! { dg-final { scan-tree-dump "\\\+ 5.*\\\)\\\).* - 5" "optimized" } }
