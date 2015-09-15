/* { dg-do compile } */
/* { dg-options "-O2 -ftree-parallelize-loops=2 -fdump-tree-gimple" } */

void
foo (int *__restrict a, int *__restrict b, int *__restrict c)
{
  int i;
  for (i = 0; i < 1000; ++i)
    c[i] = a[i] + b[i];
}

/* Check that new function does not end up in gimple dump.  */
/* { dg-final { scan-tree-dump-not "foo\\.\[\\\$_\]loopfn\\.0" "gimple" } } */
