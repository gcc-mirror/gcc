/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-gimple" } */

void __attribute__((noinline))
baz (int *p)
{
}

void
foo (void)
{
  int p[2];
  p[0] = 1;
  p[1] = 3;
  #pragma omp task firstprivate (p)
    baz (p);
}

/* Check that new function does not end up in gimple dump.  */
/* { dg-final { scan-tree-dump-not "foo\\._omp_cpyfn\\.1 \\(struct" "gimple" } } */
