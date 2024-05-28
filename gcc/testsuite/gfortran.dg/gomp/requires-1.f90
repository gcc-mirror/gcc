subroutine foo
!$omp requires unified_address
!$omp requires unified_shared_memory
!$omp requires unified_shared_memory unified_address
!$omp requires dynamic_allocators,reverse_offload
end

subroutine bar
!$omp requires unified_shared_memory unified_address
!$omp requires atomic_default_mem_order(seq_cst)
end
