! { dg-additional-options "-fdump-tree-original" }

integer, target :: tgtarray(20)
integer, pointer, contiguous :: arrayptr(:)

arrayptr => tgtarray

!$omp target update from(arrayptr)
! { dg-final { scan-tree-dump {omp target update from\(} "original" } }

!$omp target update to(arrayptr(::2))
! { dg-final { scan-tree-dump {omp target update map\(to_grid:} "original" } }

end

