/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* With a wrapping type Y + CST can land below Y.  */
unsigned int keep1 (unsigned int x, unsigned int y)
{
  unsigned int a = x > y + 7 ? x : y + 7;
  return a < y ? a : y;
}

/* This clamp has the opposite offset sign.  Use >= so phiopt forms both
   the inner MAX_EXPR and the outer MIN_EXPR.  */
int keep2 (int x, int y)
{
  int a = x >= y - 7 ? x : y - 7;
  return a < y ? a : y;
}

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
