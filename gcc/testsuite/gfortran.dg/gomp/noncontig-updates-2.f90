! { dg-additional-options "-fdump-tree-original" }

integer, allocatable :: allocarray(:)
integer, allocatable :: allocarray2(:,:)

allocate(allocarray(1:20))
allocate(allocarray2(1:20,1:20))

! This one must be noncontiguous
!$omp target update to(allocarray(::2))
! { dg-final { scan-tree-dump {omp target update map\(to_grid:} "original" } }

!$omp target update from(allocarray2(:,5:15))
! { dg-final { scan-tree-dump {omp target update from\(} "original" } }

end
