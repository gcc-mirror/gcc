/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */

extern void bar(unsigned long long);

void foo (unsigned long long n)
{
  unsigned long long i;

  #pragma omp for schedule (nonmonotonic : dynamic)
  for (i = 0; i < n; ++i)
    bar(i);
}

/* { dg-final { scan-tree-dump-times "GOMP_loop_ull_nonmonotonic_dynamic_start" 1 "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_ull_nonmonotonic_dynamic_next" 1 "ompexp" } } */
