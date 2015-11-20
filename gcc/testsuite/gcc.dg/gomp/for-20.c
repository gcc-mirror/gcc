/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */

extern void bar(int);

void foo (int n)
{
  int i;

  #pragma omp for schedule(nonmonotonic:guided)
  for (i = 0; i < n; ++i)
    bar(i);
}

/* { dg-final { scan-tree-dump-times "GOMP_loop_nonmonotonic_guided_start" 1 "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_nonmonotonic_guided_next" 1 "ompexp" } } */
