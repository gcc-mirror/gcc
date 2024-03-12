! { dg-additional-options "-fdump-tree-original" }

implicit none

integer :: a, b, close, always, to, present

!$omp target map(close)
!$omp end target

!$omp target map(always)
!$omp end target

!$omp target map(present)
!$omp end target

!$omp target map(always, close)
!$omp end target

!$omp target map(always, close, present)
!$omp end target

!$omp target map(always, close, to : always, close, a)
!$omp end target

!$omp target map(always, close, present, to : always, close, present, a)
!$omp end target


!$omp target map(to, always, close)
!$omp end target

!$omp target map(present, to, always, close)
!$omp end target

!$omp target map ( present , from : present) map(close , alloc : close) , map ( always, tofrom: always )
!$omp end target

end

! { dg-final { scan-tree-dump-not "map\\(\[^\n\r)]*close\[^\n\r)]*to:" "original" } }
! { dg-final { scan-tree-dump-not "map\\(\[^\n\r)]*close\[^\n\r)]*to:" "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:close\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:always\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:present\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:always\\) map\\(tofrom:close\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:always\\) map\\(tofrom:close\\) map\\(tofrom:present\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(always,to:always\\) map\\(always,to:close\\) map\\(always,to:a\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(always,present,to:always\\) map\\(always,present,to:close\\) map\\(always,present,to:present\\) map\\(always,present,to:a\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:to\\) map\\(tofrom:always\\) map\\(tofrom:close\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(tofrom:present\\) map\\(tofrom:to\\) map\\(tofrom:always\\) map\\(tofrom:close\\)\[\n\r]" 1 "original" } }
! { dg-final { scan-tree-dump-times "#pragma omp target map\\(present,from:present\\) map\\(alloc:close\\) map\\(always,tofrom:always\\)\[\n\r]" 1 "original" } }
