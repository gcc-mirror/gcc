/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */

extern void bar(int);

void foo (int n)
{
  int i;

  #pragma omp for schedule(guided) ordered
  for (i = 0; i < n; ++i)
    bar(i);
}

/* { dg-final { scan-tree-dump-times "GOMP_loop_ordered_guided_start" 1 "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_ordered_guided_next" 1 "ompexp" } } */
/* { dg-final { cleanup-tree-dump "ompexp" } } */
