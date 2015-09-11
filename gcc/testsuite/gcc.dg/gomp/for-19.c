/* Verify that if GOMP_parallel_loop_dynamic_start is used, variables
   mentioned in the INIT, COND and INCR expressions aren't unnecessarily
   copied to the omp_fn function.  */
/* { dg-do compile } */
/* { dg-options "-O -fopenmp -fdump-tree-gimple" } */

void foo (int *a, int i, int j, int k, int l, int m)
{
#pragma omp parallel for num_threads (3 * i) schedule (dynamic, i * 4)
  for (j = 0; j <= (6 * l + 4 * k); j++)
    a[j] = 1;
#pragma omp parallel for num_threads (3 * i) schedule (dynamic, i * 4)
  for (j = m; j <= l; j += (k + l - m))
    a[j] = 1;
}

/* { dg-final { scan-tree-dump-times "shared\\(a\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "shared\\(k\\)" 0 "gimple" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "shared\\(l\\)" 0 "gimple" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "shared\\(m\\)" 0 "gimple" { xfail *-*-* } } } */
