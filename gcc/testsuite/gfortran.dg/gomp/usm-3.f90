! { dg-do compile }
! { dg-additional-options "-foffload-memory=unified -fdump-tree-usm_transform" }

subroutine foo()
  implicit none
  integer, allocatable :: var1

  allocate(var1)

end subroutine

! { dg-final { scan-tree-dump-times "omp_alloc" 1 "usm_transform"  } } 
! { dg-final { scan-tree-dump-times "omp_free" 1 "usm_transform"  } } 