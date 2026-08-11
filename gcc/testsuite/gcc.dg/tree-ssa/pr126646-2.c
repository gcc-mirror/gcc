/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f (int a, int b)
{
  int x = __builtin_abs (a);
  int y = __builtin_abs (b);
  int p = x < 1 ? x : 1;
  int q = y < 1 ? y : 1;
  return p | q;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "optimized" } } */
