/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ull_start \[^\n\r]*, 3, 3, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ull_nonmonotonic_guided_next " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_workshare_task_reduction_unregister \\(0\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "optimized" } } */

unsigned long long int j;
void bar (unsigned long long int *);

void
foo (unsigned long long int a, unsigned long long int b, unsigned long long int c)
{
  unsigned long long int i;
  #pragma omp parallel
  #pragma omp for reduction (task, *: j) schedule (nonmonotonic: guided, 3)
  for (i = a; i < b; i += c)
    {
      j++;
      bar (&j);
    }
}
