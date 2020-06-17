int i, j;

void
foo ()
{
  int v;
  #pragma omp atomic release
  i = i + 1;
  #pragma omp atomic read
  v = j;
}

#pragma omp requires atomic_default_mem_order (acq_rel)	/* { dg-error "'atomic_default_mem_order' clause used lexically after first 'atomic' construct without memory order clause" } */
