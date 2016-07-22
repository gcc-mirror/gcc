/* { dg-do compile } */
/* { dg-options "-fopenacc -fdump-tree-gimple" } */

#include <assert.h>

int
main ()
{
  int i, v1 = 0, n = 100;

#pragma acc parallel loop reduction(+:v1)
  for (i = 0; i < n; i++)
    v1++;

  assert (v1 == n);

#pragma acc kernels loop reduction(+:v1)
  for (i = 0; i < n; i++)
    v1++;

  assert (v1 == n);

  return 0;
}

/* { dg-final { scan-tree-dump-times "omp target oacc_parallel reduction.+:v1. map.tofrom:v1" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "acc loop reduction.+:v1. private.i." 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "omp target oacc_kernels map.force_tofrom:n .len: 4.. map.force_tofrom:v1 .len: 4.." 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "acc loop reduction.+:v1. private.i." 1 "gimple" } } */
