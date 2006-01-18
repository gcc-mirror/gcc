/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-lower" } */

extern void bar(int);

void foo (int n)
{
  int i;

  #pragma omp for schedule(dynamic) ordered
  for (i = 0; i < n; ++i)
    bar(i);
}

/* { dg-final { scan-tree-dump-times "GOMP_loop_ordered_dynamic_start" 1 "lower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_ordered_dynamic_next" 1 "lower" } } */
/* { dg-final { cleanup-tree-dump "lower" } } */
