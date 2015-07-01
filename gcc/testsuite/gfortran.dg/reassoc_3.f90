! { dg-do compile }
! { dg-options "-O -ffast-math -fdump-tree-original -fdump-tree-optimized" }

! Verify we associate properly during folding
! Verify we propagate constants in the presence of PAREN_EXPR

function test(a)
  real b, c, d
  c = a
  d = 5
  b = (c + 5 - c)
  b = (c + d - c)
  test = a + b - 5
end

! { dg-final { scan-tree-dump "b = 5" "original" } }
! { dg-final { scan-tree-dump-times " = " 1 "optimized" } }
