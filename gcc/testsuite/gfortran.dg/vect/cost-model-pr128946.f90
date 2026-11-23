! { dg-do compile }
! { dg-additional-options "-fvect-cost-model=dynamic -fdump-tree-slp2-details" }

real(8) function radd_v5 (a,b,c,d) result(res)
  real(8), value :: a, b, c, d
  real(8) :: tmp
  res = (a + b) + (c + d)
end function radd_v5

! { dg-final { scan-tree-dump-not "optimized: basic block part vectorized" "slp2" } }
