/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ordered_start \[^\n\r]*, (?:2147483650|-2147483646), 4, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_ordered_start " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_ordered_end " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ordered_dynamic_next " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_workshare_task_reduction_unregister \\(0\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "optimized" } } */

int j;
void bar (int *);

void
foo (int a, int b, int c)
{
  int i;
  #pragma omp parallel
  #pragma omp for ordered reduction (task, *: j) schedule (dynamic, 4)
  for (i = a; i < b; i += c)
    {
      bar (&j);
      #pragma omp ordered
      j++;
    }
}
