/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */

extern void bar(int);

void foo (int n)
{
  int i;

  #pragma omp for schedule(static) ordered
  for (i = 0; i < n; ++i)
    bar(i);
}

/* { dg-final { scan-tree-dump-times "GOMP_loop_ordered_static_start" 1 "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_ordered_static_next" 1 "ompexp" } } */
