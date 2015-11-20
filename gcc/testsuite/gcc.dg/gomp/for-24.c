/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-ssa" } */

extern void bar(int);

void foo (void)
{
  int i;

  #pragma omp parallel for schedule (nonmonotonic : dynamic, 4)
  for (i = 0; i < 37; ++i)
    bar(i);
}

/* { dg-final { scan-tree-dump-times "GOMP_parallel_loop_nonmonotonic_dynamic" 1 "ssa" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_nonmonotonic_dynamic_start" 0 "ssa" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_nonmonotonic_dynamic_next" 2 "ssa" } } */
