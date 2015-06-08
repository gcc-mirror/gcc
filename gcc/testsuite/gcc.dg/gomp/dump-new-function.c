/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-gimple" } */

int
main (void)
{
#pragma omp parallel
  {
    extern void foo (void);
    foo ();
  }
  return 0;
}

/* Check that new function does not end up in gimple dump.  */
/* { dg-final { scan-tree-dump-not "main\\._omp_fn\\.0" "gimple" } } */
