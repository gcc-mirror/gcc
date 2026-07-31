/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* MIN (MAX (X, Y + CST), Y) is Y for a positive CST, and the dual
   MAX (MIN (X, Y + CST), Y) is Y for a negative one.  */

int f1 (int x, int y)
{
  int a = x > y + 7 ? x : y + 7;
  return a < y ? a : y;
}

long f2 (long x, long y)
{
  long a = x < y - 9 ? x : y - 9;
  return a > y ? a : y;
}

/* f1 and f2 collapse to a bare return of Y.  */
/* { dg-final { scan-tree-dump-times "return y_" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MAX_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "optimized" } } */
