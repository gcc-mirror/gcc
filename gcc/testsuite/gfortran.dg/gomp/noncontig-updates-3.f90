! { dg-additional-options "-fdump-tree-original" }

integer, allocatable :: allocarray(:,:)

allocate(allocarray(1:20,1:20))

! This one could possibly be handled as a contiguous update - but isn't,
! for now.
!$omp target update to(allocarray(1:20,5:15))
! { dg-final { scan-tree-dump {omp target update map\(to_grid:} "original" } }

!$omp target update from(allocarray(:,5:15:2))
! { dg-final { scan-tree-dump {omp target update map\(from_grid:} "original" } }

end

