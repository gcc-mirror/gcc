! { dg-additional-options "-fdump-tree-original" }

implicit none

integer :: a, b, b1, b2, b3, b4, b5, b6

!$omp target map(a)
!$omp end target

!$omp target map(to : a)
!$omp end target

!$omp target map(always to: a)
!$omp end target
!$omp target map(always, to: a)
!$omp end target
!$omp target map(close to: a)
!$omp end target
!$omp target map(close, to: a)
!$omp end target

!$omp target map(close always to:b1)
!$omp end target
!$omp target map(close, always to:b2)
!$omp end target
!$omp target map(close, always, to:b3)
!$omp end target
!$omp target map(always close to:b4)
!$omp end target
!$omp target map(always, close to:b5)
!$omp end target
!$omp target map(always, close, to:b6)
!$omp end target


!$omp target map (always to : a) map (close to : b)
!$omp end target

end

! { dg-final { scan-tree-dump-not "map\\(\[^\n\r)]*close\[^\n\r)]*to:" "original" } }

! { dg-final { scan-tree-dump-times "#pragma omp target map\\(always,to:" 9 "original" } }

! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:b1\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:b2\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:b3\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:b4\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:b5\\)" "original" } }
! { dg-final { scan-tree-dump "#pragma omp target map\\(always,to:b6\\)" "original" } }
