#pragma omp requires	/* { dg-error "requires at least one clause" } */
#pragma omp requires unified_shared_memory,unified_shared_memory	/* { dg-error "too many 'unified_shared_memory' clauses" } */
#pragma omp requires unified_address	unified_address	/* { dg-error "too many 'unified_address' clauses" } */
#pragma omp requires reverse_offload reverse_offload	/* { dg-error "too many 'reverse_offload' clauses" } */
#pragma omp requires foobarbaz	/* { dg-error "expected 'unified_address', 'unified_shared_memory', 'dynamic_allocators', 'reverse_offload' or 'atomic_default_mem_order' clause" } */
#pragma omp requires dynamic_allocators , dynamic_allocators	/* { dg-error "too many 'dynamic_allocators' clauses" } */
#pragma omp requires atomic_default_mem_order(seq_cst) atomic_default_mem_order(seq_cst)	/* { dg-error "too many 'atomic_default_mem_order' clauses" } */
#pragma omp requires atomic_default_mem_order (seq_cst)	/* { dg-error "more than one 'atomic_default_mem_order' clause in a single compilation unit" } */
