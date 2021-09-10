/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

#pragma omp requires atomic_default_mem_order(acq_rel)

void
foo ()
{
  int i, v;

#pragma omp atomic read
  i = v;

#pragma acc atomic read
  i = v;

#pragma omp atomic write
  i = v;

#pragma acc atomic write
  i = v;

#pragma omp atomic update
  i += 1;

#pragma acc atomic update
  i += 1;

#pragma omp atomic capture
  v = i += 1;

#pragma acc atomic capture
  v = i += 1;
#pragma acc atomic update capture
  v = i += 1;
}

/* { dg-final { scan-tree-dump-times "i = #pragma omp atomic read acquire" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "i = #pragma omp atomic read relaxed" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp atomic acq_rel" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp atomic release" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp atomic relaxed" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture acq_rel" 1  "original" } } */
/* { dg-final { scan-tree-dump-times "v = #pragma omp atomic capture relaxed" 2 "original" } } */
