! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }

module mod
implicit none

integer i, j, k, l, m, n
contains

subroutine foo ()
  !$omp atomic release
  i = i + 1
end
end module

module m2
use mod
implicit none
!$omp requires atomic_default_mem_order (acq_rel)

contains
subroutine bar ()
  integer v
  !$omp atomic
  j = j + 1
  !$omp atomic update
  k = k + 1
  !$omp atomic read
  v = l
  !$omp atomic write
  m = v
  !$omp atomic capture
  n = n + 1; v = n
end

! { dg-final { scan-tree-dump-times "#pragma omp atomic release" 5 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture acq_rel" 1 "original" } }
! { dg-final { scan-tree-dump-times "v = #pragma omp atomic read acquire" 1 "original" } }

subroutine foobar()
  integer :: aa, bb, cc, dd, ee, ff, gg, hh, ii, jj, kk, nn, oo, pp, qq

  !$omp atomic compare
    if (ii == jj) ii = kk

!  #pragma omp atomic release
!    TARGET_EXPR <D.4241, &ii> = *TARGET_EXPR <D.4241, &ii> == jj \\? kk : *TARGET_EXPR <D.4241, &ii>;
!
! { dg-final { scan-tree-dump-times "TARGET_EXPR <D.\[0-9\]+, &ii> = \\*TARGET_EXPR <D.\[0-9\]+, &ii> == jj \\? kk : \\*TARGET_EXPR <D.\[0-9\]+, &ii>;" 1 "original" } }

  !$omp atomic compare, capture
    if (nn == oo) then
      nn = pp
    else
      qq = nn
    endif

!  TARGET_EXPR <D.4244, 0> = #pragma omp atomic capture acq_rel
!    TARGET_EXPR <D.4242, &nn> = NON_LVALUE_EXPR <TARGET_EXPR <D.4243, 0> = *TARGET_EXPR <D.4242, &nn> == oo> ? pp : *TARGET_EXPR <D.4242, &nn>;, if (TARGET_EXPR <D.4243, 0>)
!    {
!      <<< Unknown tree: void_cst >>>
!    }
!  else
!    {
!      qq = TARGET_EXPR <D.4244, 0>;
!    };
!
! { dg-final { scan-tree-dump-times "TARGET_EXPR <D.\[0-9\]+, 0> = #pragma omp atomic capture acq_rel" 1 "original" } }
! { dg-final { scan-tree-dump-times "TARGET_EXPR <D.\[0-9\]+, &nn> = NON_LVALUE_EXPR <TARGET_EXPR <D.\[0-9\]+, 0> = \\*TARGET_EXPR <D.\[0-9\]+, &nn> == oo> \\? pp : \\*TARGET_EXPR <D.\[0-9\]+, &nn>;, if \\(TARGET_EXPR <D.\[0-9\]+, 0>\\)" 1 "original" } }
! { dg-final { scan-tree-dump-times "<<< Unknown tree: void_cst >>>" 1 "original" } }
! { dg-final { scan-tree-dump-times "qq = TARGET_EXPR <D.\[0-9\]+, 0>;" 1 "original" } }

  !$omp atomic capture compare
    aa = bb
    if (bb == cc) bb = dd

!  aa = #pragma omp atomic capture acq_rel
!    TARGET_EXPR <D.4245, &bb> = *TARGET_EXPR <D.4245, &bb> == cc ? dd : *TARGET_EXPR <D.4245, &bb>;
!
! { dg-final { scan-tree-dump-times "aa = #pragma omp atomic capture acq_rel" 1 "original" } }
! { dg-final { scan-tree-dump-times "TARGET_EXPR <D.\[0-9\]+, &bb> = \\*TARGET_EXPR <D.\[0-9\]+, &bb> == cc \\? dd : \\*TARGET_EXPR <D.\[0-9\]+, &bb>;" 1 "original" } }

  !$omp atomic capture compare
    if (ee == ff) ee = gg
    hh = ee

!  hh = #pragma omp atomic capture acq_rel
!    TARGET_EXPR <D.4246, &ee> = *TARGET_EXPR <D.4246, &ee> == ff ? gg : *TARGET_EXPR <D.4246, &ee>;
!
! { dg-final { scan-tree-dump-times "hh = #pragma omp atomic capture acq_rel" 1 "original" } }
! { dg-final { scan-tree-dump-times "TARGET_EXPR <D.\[0-9\]+, &ee> = \\*TARGET_EXPR <D.\[0-9\]+, &ee> == ff \\? gg : \\*TARGET_EXPR <D.\[0-9\]+, &ee>;" 1 "original" } }
end
end module
