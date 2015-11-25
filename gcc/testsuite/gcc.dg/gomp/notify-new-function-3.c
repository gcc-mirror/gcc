/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-ompexpssa" } */

void
foo (int *__restrict a, int *__restrict b, int *__restrict c)
{
  int i;
  for (i = 0; i < 1000; ++i)
    c[i] = a[i] + b[i];
}


/* Check for new function notification in ompexpssa dump.  */
/* { dg-final { scan-tree-dump-times "Added new ssa gimple function foo\\.\[\\\$_\]loopfn\\.0 to callgraph" 1 "ompexpssa2" } } */
