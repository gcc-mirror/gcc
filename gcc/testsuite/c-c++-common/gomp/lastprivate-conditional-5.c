/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-ompexp" } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_start " 3 "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_loop_end_nowait " 3 "ompexp" } } */

int r;

void
foo (int *a)
{
  #pragma omp for nowait lastprivate(conditional: r)
  for (int i = 0; i < 64; ++i)
    if (a[i])
      r = a[i];
}

void
bar (int *a)
{
  #pragma omp for nowait lastprivate(conditional: r) schedule (static, 4)
  for (int i = 0; i < 64; ++i)
    if (a[i])
      r = a[i];
}

void
baz (int *a)
{
  #pragma omp for nowait lastprivate(conditional: r) schedule (runtime)
  for (int i = 0; i < 64; ++i)
    if (a[i])
      r = a[i];
}
