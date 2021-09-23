! { dg-additional-options "-fdump-tree-original" }

implicit none

integer :: a, b, close, always, to

!$omp target map(close)
!$omp end target

!$omp target map(always)
!$omp end target

!$omp target map(always, close)
!$omp end target

!$omp target map(always, close, to : always, close, a)
!$omp end target

!$omp target map(to, always, close)
!$omp end target

end

! { dg-final { scan-tree-dump-not "map\\(\[^\n\r)]*close\[^\n\r)]*to:" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:always\\) map\\(always,to:close\\) map\\(always,to:a\\)" "original" } }
! { dg-final { scan-tree-dump-not "map\\(\[^\n\r)]*close\[^\n\r)]*to:" "original" } }
