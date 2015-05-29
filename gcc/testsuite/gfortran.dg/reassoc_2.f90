! { dg-do compile }
! { dg-options "-O3 -ffast-math -fdump-tree-optimized" }

! Make sure that FRE does not replace c with b in d = c - 5

function test(a)
  real a, b, c, d
  b = a + 5.
  c = (a + 5.)
  d = c - 5.
  call foo(b)
  test = d
end

! { dg-final { scan-tree-dump "- 5" "optimized" } }
