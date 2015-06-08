/* { dg-do compile } */
/* { dg-options "-O2 -fopenmp -fdump-tree-ompexp" } */

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


/* Check for new function notification in ompexp dump.  */
/* { dg-final { scan-tree-dump-times "Added new low gimple function main\\._omp_fn\\.0 to callgraph" 1 "ompexp" } } */
