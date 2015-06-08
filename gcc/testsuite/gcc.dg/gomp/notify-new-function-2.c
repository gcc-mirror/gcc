/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-omplower" } */

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

/* Check for new function notification in omplower dump.  */
/* { dg-final { scan-tree-dump-times "Added new high gimple function foo\\._omp_cpyfn\\.1 to callgraph" 1 "omplower" } } */
