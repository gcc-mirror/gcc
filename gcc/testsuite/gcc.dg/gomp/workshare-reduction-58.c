/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_doacross_start \[^\n\r]*, (?:2147483651|-2147483645), 1, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_doacross_post " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_doacross_wait " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_guided_next " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_workshare_task_reduction_unregister \\(0\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "optimized" } } */

int j;
void bar (int *);

void
foo (int a, int b, int c)
{
  int i;
  #pragma omp parallel
  #pragma omp for ordered(1) reduction (task, *: j) schedule (guided)
  for (i = a; i < b; i += c)
    {
      bar (&j);
      #pragma omp ordered depend(sink: i - 1)
      j++;
      #pragma omp ordered depend(source)
    }
}
