#pragma omp requires unified_shared_memory,unified_address,reverse_offload
void
foo (void)
{
  #pragma omp target
  ;
  #pragma omp requires unified_shared_memory	/* { dg-error "'unified_shared_memory' clause used lexically after first target construct or offloading API" } */
}

#pragma omp requires unified_address	/* { dg-error "'unified_address' clause used lexically after first target construct or offloading API" } */
#pragma omp requires reverse_offload	/* { dg-error "'reverse_offload' clause used lexically after first target construct or offloading API" } */

/* { dg-prune-output "not supported yet" } */
