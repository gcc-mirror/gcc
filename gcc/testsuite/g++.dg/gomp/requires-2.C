int i;

void
foo ()
{
  #pragma omp requires unified_address				// { dg-error "may only be used at file or namespace scope" }
  #pragma omp requires unified_shared_memory			// { dg-error "may only be used at file or namespace scope" }
  #pragma omp requires unified_shared_memory unified_address	// { dg-error "may only be used at file or namespace scope" }
  #pragma omp requires dynamic_allocators,reverse_offload	// { dg-error "may only be used at file or namespace scope" }
  #pragma omp requires atomic_default_mem_order(seq_cst)	// { dg-error "may only be used at file or namespace scope" }
  if (0)
    #pragma omp requires unified_address			// { dg-error "may only be used at file or namespace scope" }
    i++;
  if (0)
    #pragma omp requires atomic_default_mem_order(seq_cst)	// { dg-error "may only be used at file or namespace scope" }
    i++;
}

struct S {
  int s;
  #pragma omp requires unified_address				// { dg-error "may only be used at file or namespace scope" }
};
