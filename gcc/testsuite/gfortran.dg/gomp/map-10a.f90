! { dg-additional-options "-fdump-tree-omplower" }

! If enter data adds a (GOMP_MAP_)POINTER attachment, exit data needs to remove
! it again. If not there can be all kind of issues, in particular when
! stack memory was mapped, reused later and mapped again.

subroutine test_aa (aa2, aa3)
  integer(kind=4), allocatable :: aa1, aa2, aa3
  optional :: aa3
  !$omp target enter data map(aa1)
  !$omp target exit data map(aa1)
  !$omp target enter data map(aa2)
  !$omp target exit data map(aa2)
  !$omp target enter data map(aa3)
  !$omp target exit data map(aa3)
end

subroutine test_pp (pp2, pp3)
  integer(kind=4), allocatable :: pp1, pp2, pp3
  optional :: pp3
  !$omp target enter data map(pp1)
  !$omp target exit data map(pp1)
  !$omp target enter data map(pp2)
  !$omp target exit data map(pp2)
  !$omp target enter data map(pp3)
  !$omp target exit data map(pp3)
end

subroutine test_pprelease (rp2, rp3)
  integer(kind=4), allocatable :: rp1, rp2, rp3
  optional :: rp3
  !$omp target enter data map(rp1)
  !$omp target exit data map(release:rp1)
  !$omp target enter data map(rp2)
  !$omp target exit data map(release:rp2)
  !$omp target enter data map(rp3)
  !$omp target exit data map(release:rp3)
end

subroutine test_ppdelete (dp2, dp3)
  integer(kind=4), allocatable :: dp1, dp2, dp3
  optional :: dp3
  !$omp target enter data map(dp1)
  !$omp target exit data map(delete:dp1)
  !$omp target enter data map(dp2)
  !$omp target exit data map(delete:dp2)
  !$omp target enter data map(dp3)
  !$omp target exit data map(delete:dp3)
end


! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:\\*aa1.\[0-9\]+_\[0-9\]+ \\\[len: 4\\\]\\) map\\(alloc:aa1 \\\[pointer assign, bias: 0\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:\\*aa1.\[0-9\]+_\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:aa1 \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:\\*_\[0-9\]+ \\\[len: 4\\\]\\) map\\(alloc:\\*aa2.\[0-9\]+_\[0-9\]+ \\\[pointer assign, bias: 0\\\]\\) map\\(alloc:aa2 \\\[pointer assign, bias: 0\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:\\*_\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:aa2 \\\[len: .\\\]\\) map\\(release:\\*aa2.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:\\*D.\[0-9\]+ \\\[len: 4\\\]\\) map\\(alloc:\\*aa3.\[0-9\]+_\[0-9\]+ \\\[pointer assign, bias: 0\\\]\\) map\\(alloc:aa3 \\\[pointer assign, bias: 0\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:\\*D.\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:aa3 \\\[len: .\\\]\\) map\\(release:\\*aa3.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:\\*pp1.\[0-9\]+_\[0-9\]+ \\\[len: 4\\\]\\) map\\(alloc:pp1 \\\[pointer assign, bias: 0\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:\\*pp1.\[0-9\]+_\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:pp1 \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:\\*_\[0-9\]+ \\\[len: 4\\\]\\) map\\(alloc:\\*pp2.\[0-9\]+_\[0-9\]+ \\\[pointer assign, bias: 0\\\]\\) map\\(alloc:pp2 \\\[pointer assign, bias: 0\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:\\*_\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:pp2 \\\[len: .\\\]\\) map\\(release:\\*pp2.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target enter data map\\(to:\\*D.\[0-9\]+ \\\[len: 4\\\]\\) map\\(alloc:\\*pp3.\[0-9\]+_\[0-9\]+ \\\[pointer assign, bias: 0\\\]\\) map\\(alloc:pp3 \\\[pointer assign, bias: 0\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(from:\\*D.\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:pp3 \\\[len: .\\\]\\) map\\(release:\\*pp3.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(release:rp1 \\\[len: .\\\]\\) map\\(release:\\*rp1.\[0-9\]+_\[0-9\]+ \\\[len: 4\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(release:rp2 \\\[len: .\\\]\\) map\\(release:\\*rp2.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\) map\\(release:\\*_\[0-9\]+ \\\[len: 4\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(release:\\*D.\[0-9\]+ \\\[len: 4\\\]\\) map\\(release:rp3 \\\[len: .\\\]\\) map\\(release:\\*rp3.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(delete:dp1 \\\[len: .\\\]\\) map\\(delete:\\*dp1.\[0-9\]+_\[0-9\]+ \\\[len: 4\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(delete:dp2 \\\[len: .\\\]\\) map\\(delete:\\*dp2.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\) map\\(delete:\\*_\[0-9\]+ \\\[len: 4\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "#pragma omp target exit data map\\(delete:\\*D.\[0-9\]+ \\\[len: 4\\\]\\) map\\(delete:dp3 \\\[len: .\\\]\\) map\\(delete:\\*dp3.\[0-9\]+_\[0-9\]+ \\\[len: .\\\]\\)" "omplower" } }
