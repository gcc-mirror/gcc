/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* The test case should produce only one min expr.  */
/* umin(a,1) | umin(b,1) -> umin(a|b, 1).  */

unsigned min_or (unsigned a, unsigned b)
{
  unsigned t = 1;
  a = a < t ? a : t;
  b = b < t ? b : t;
  return a | b;
}

/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "optimized" } } */
