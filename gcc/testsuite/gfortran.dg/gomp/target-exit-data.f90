! { dg-do compile }
! { dg-additional-options "-fdump-tree-omplower" }
!
! PR middle-end/94635

integer, allocatable :: one(:), two(:), three(:)

!$omp target enter data map(alloc:one)
!$omp target enter data map(alloc:two)
!$omp target enter data map(to:three)

! ...
!$omp target exit data map(delete:one)
!$omp target exit data map(release:two)
!$omp target exit data map(from:three)
end

! { dg-final { scan-tree-dump "omp target exit data map\\(delete:.*\\) map\\(delete:one \\\[len: .*\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "omp target exit data map\\(release:.*\\) map\\(release:two \\\[len: .*\\\]\\)" "omplower" } }
! { dg-final { scan-tree-dump "omp target exit data map\\(from:.*\\) map\\(release:three \\\[len: .*\\\]\\)" "omplower" } }
