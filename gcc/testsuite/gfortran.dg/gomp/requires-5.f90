subroutine bar
!$omp requires atomic_default_mem_order(seq_cst)
!$omp requires unified_shared_memory
end

subroutine foo
!$omp requires unified_shared_memory
!$omp requires unified_shared_memory
!$omp requires atomic_default_mem_order(relaxed)
!$omp requires atomic_default_mem_order(relaxed)
!$omp requires atomic_default_mem_order(seq_cst) ! { dg-error "overrides a previous 'atomic_default_mem_order\\(relaxed\\)'" }
  !$omp target
  !$omp end target
end
