!$omp requires	! { dg-error "Expected UNIFIED_ADDRESS, UNIFIED_SHARED_MEMORY, SELF_MAPS, DYNAMIC_ALLOCATORS, REVERSE_OFFLOAD, or ATOMIC_DEFAULT_MEM_ORDER clause" }
!$omp requires unified_shared_memory,unified_shared_memory	! { dg-error "Duplicated 'unified_shared_memory' clause" }
!$omp requires unified_address	unified_address	! { dg-error "Duplicated 'unified_address' clause" }
!$omp requires reverse_offload reverse_offload	! { dg-error "Duplicated 'reverse_offload' clause" }
!$omp requires foobarbaz	! { dg-error "Expected UNIFIED_ADDRESS, UNIFIED_SHARED_MEMORY, SELF_MAPS, DYNAMIC_ALLOCATORS, REVERSE_OFFLOAD, or ATOMIC_DEFAULT_MEM_ORDER clause" }
!$omp requires dynamic_allocators , dynamic_allocators	! { dg-error "Duplicated 'dynamic_allocators' clause" }
!$omp requires atomic_default_mem_order(seq_cst) atomic_default_mem_order(seq_cst)	! { dg-error "Duplicated 'atomic_default_mem_order' clause" }
!$omp requires atomic_default_mem_order (seq_cst)
!$omp requires atomic_default_mem_order (seq_cst)
!$omp requires atomic_default_mem_order (acq_rel) ! { dg-error "overrides a previous 'atomic_default_mem_order\\(seq_cst\\)'" }
!$omp requires atomic_default_mem_order (foo) ! { dg-error "Expected ACQ_REL, ACQUIRE, RELAXED, RELEASE or SEQ_CST for ATOMIC_DEFAULT_MEM_ORDER clause" }
end
