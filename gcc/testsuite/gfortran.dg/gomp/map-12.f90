! { dg-additional-options "-fdump-tree-omplower -fdump-tree-original" }

subroutine foo
  implicit none
  integer :: a, b, b1

  !$omp target data map(tofrom:b1)
    block; end block
  !$omp target data map(close,tofrom:b1)
    block; end block
  !$omp target data map(close always,tofrom:b1)
    block; end block
  !$omp target data map(close always,tofrom:b1)
    block; end block
  !$omp target data map(close present,tofrom:b1)
    block; end block
  !$omp target data map(close present,tofrom:b1)
    block; end block
  !$omp target data map(always close present,tofrom:b1)
    block; end block
  !$omp target data map(always close present,tofrom:b1)
    block; end block

  !$omp target enter data map(alloc: a) map(to:b) map(tofrom:b1)
  !$omp target enter data map(close, alloc: a) map(close,to:b) map(close,tofrom:b1)
  !$omp target enter data map(always,alloc: a) map(always,to:b) map(close always,tofrom:b1)
  !$omp target enter data map(always,close,alloc: a) map(close,always,to:b) map(close always,tofrom:b1)
  !$omp target enter data map(present,alloc: a) map(present,to:b) map(close present,tofrom:b1)
  !$omp target enter data map(present,close,alloc: a) map(close,present,to:b) map(close present,tofrom:b1)
  !$omp target enter data map(present,always,alloc: a) map(always,present,to:b) map(always close present,tofrom:b1)
  !$omp target enter data map(present,always,close,alloc: a) map(close,present,always,to:b) map(always close present,tofrom:b1)

  !$omp target exit data map(delete: a) map(release:b) map(from:b1)
  !$omp target exit data map(close,delete: a) map(close,release:b) map(close,from:b1)
  !$omp target exit data map(always,delete: a) map(always,release:b) map(close always,from:b1)
  !$omp target exit data map(always,close,delete: a) map(close,always,release:b) map(close always,from:b1)
  !$omp target exit data map(present,delete: a) map(present,release:b) map(close present,from:b1)
  !$omp target exit data map(present,close,delete: a) map(close,present,release:b) map(close present,from:b1)
  !$omp target exit data map(present,always,delete: a) map(always,present,release:b) map(always close present,from:b1)
  !$omp target exit data map(present,always,close,delete: a) map(close,present,always,release:b) map(always close present,from:b1)
end subroutine

! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,tofrom:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(present,tofrom:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,present,tofrom:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(alloc:a\\) map\\(to:b\\) map\\(to:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(alloc:a\\) map\\(always,to:b\\) map\\(always,to:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(present,alloc:a\\) map\\(present,to:b\\) map\\(present,to:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(present,alloc:a\\) map\\(always,present,to:b\\) map\\(always,present,to:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(delete:a\\) map\\(release:b\\) map\\(from:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(delete:a\\) map\\(release:b\\) map\\(always,from:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(delete:a\\) map\\(release:b\\) map\\(present,from:b1\\)\[\r\n\]" 2 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(delete:a\\) map\\(release:b\\) map\\(always,present,from:b1\\)\[\r\n\]" 2 "original" } }


! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(tofrom:b1 \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,tofrom:b1 \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(force_present:b1 \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target data map\\(always,present,tofrom:b1 \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(to:b \\\[len: 4\\\]\\) map\\(to:b1 \\\[len: 4\\\]\\) map\\(alloc:a \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(always,to:b \\\[len: 4\\\]\\) map\\(always,to:b1 \\\[len: 4\\\]\\) map\\(alloc:a \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(force_present:a \\\[len: 4\\\]\\) map\\(force_present:b \\\[len: 4\\\]\\) map\\(force_present:b1 \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target enter data map\\(force_present:a \\\[len: 4\\\]\\) map\\(always,present,to:b \\\[len: 4\\\]\\) map\\(always,present,to:b1 \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(from:b1 \\\[len: 4\\\]\\) map\\(delete:a \\\[len: 4\\\]\\) map\\(release:b \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(always,from:b1 \\\[len: 4\\\]\\) map\\(delete:a \\\[len: 4\\\]\\) map\\(release:b \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(force_present:b1 \\\[len: 4\\\]\\) map\\(delete:a \\\[len: 4\\\]\\) map\\(release:b \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
! { dg-final { scan-tree-dump-times "#pragma omp target exit data map\\(always,present,from:b1 \\\[len: 4\\\]\\) map\\(delete:a \\\[len: 4\\\]\\) map\\(release:b \\\[len: 4\\\]\\)\[\r\n\]" 2 "omplower" } }
