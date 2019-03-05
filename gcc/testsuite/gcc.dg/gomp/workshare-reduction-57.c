/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ull_doacross_start \[^\n\r]*, (?:2147483650|-2147483646), 1, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_end " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_doacross_ull_post " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_doacross_ull_wait " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_loop_ull_dynamic_next " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_workshare_task_reduction_unregister \\(0\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_GOMP_parallel " 1 "optimized" } } */

unsigned long long int j;
void bar (unsigned long long int *);

void
foo (unsigned long long int a, unsigned long long int b, unsigned long long int c)
{
  unsigned long long int i;
  #pragma omp parallel
  #pragma omp for ordered(1) reduction (task, *: j) schedule (dynamic)
  for (i = a; i < b; i += c)
    {
      bar (&j);
      #pragma omp ordered depend(sink: i - 1)
      j++;
      #pragma omp ordered depend(source)
    }
}
