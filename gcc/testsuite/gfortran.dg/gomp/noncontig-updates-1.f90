! { dg-additional-options "-fdump-tree-original" }

integer :: basicarray(100)
integer, allocatable :: allocarray(:)

allocate(allocarray(1:20))

!$omp target update to(basicarray)

!$omp target update from(basicarray(:))

!$omp target update to(allocarray)

!$omp target update from(allocarray(:))

end

! { dg-final { scan-tree-dump-times {omp target update from\(} 2 "original" } }
! { dg-final { scan-tree-dump-times {omp target update to\(} 2 "original" } }
