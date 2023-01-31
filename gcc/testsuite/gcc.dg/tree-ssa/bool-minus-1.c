/* { dg-options "-O2 -fdump-tree-optimized" } */
_Bool
foo (_Bool a)
{
  int c = 1 - a;
  return c;
}

/* { dg-final { scan-tree-dump-times "1 - " 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~a" 1 "optimized" } } */

