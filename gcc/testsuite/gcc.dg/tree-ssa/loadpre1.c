/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre-stats" } */
int foo(int *a, int argc)
{
  int c;
  int d, e;

  /* Should be able to eliminate the second load of *a along the main path. */
  d = *a;
  if (argc)
    {
      a = &c;
    }
  e = *a;
  return d + e;
}
/* { dg-final { scan-tree-dump-times "Eliminated: 1" 1 "pre" } } */
/* { dg-final { cleanup-tree-dump "pre" } } */
