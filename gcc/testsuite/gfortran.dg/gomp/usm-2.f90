! { dg-do compile }
! { dg-additional-options "-fdump-tree-usm_transform" }

!$omp requires unified_shared_memory
end

subroutine foo()
  implicit none
  integer, allocatable :: var1

  allocate(var1)

end subroutine

! { dg-final { scan-tree-dump-times "omp_alloc" 1 "usm_transform"  } } 
! { dg-final { scan-tree-dump-times "omp_free" 1 "usm_transform"  } } 