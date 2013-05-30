! { dg-do compile }
! { dg-options "-O1 -fdump-tree-optimized" }
!
! PR middle-end/57073
! See also PR 57073
!
real function f(k)
  integer, value :: k
  f = (-1.0)**k
end

! { dg-final { scan-tree-dump-not "__builtin_powif"  "optimized" } }
! { dg-final { scan-tree-dump "powi_cond_\[0-9\] = k_\[0-9\]\\(D\\) & 1;"  "optimized" } }
! { dg-final { scan-tree-dump "powi_\[0-9\] = powi_cond_\[0-9\] \\? -1.0e\\+0 : 1.0e\\+0;"  "optimized" } }
! { dg-final { cleanup-tree-dump "optimized" } }
