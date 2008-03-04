! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-optimized" }

function test(b)
  real a
  a = (b + 5.) - 5.
  test = a
end

! { dg-final { scan-tree-dump "\\\+ 5.*\\\)\\\) - 5" "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
