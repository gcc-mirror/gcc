#pragma omp requires unified_address
#pragma omp requires unified_shared_memory
#pragma omp requires unified_shared_memory unified_address
#pragma omp requires dynamic_allocators,reverse_offload

void
foo ()
{
}

#pragma omp requires unified_shared_memory unified_address
#pragma omp requires atomic_default_mem_order(seq_cst)

/* { dg-prune-output "not supported yet" } */
