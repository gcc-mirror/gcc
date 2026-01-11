/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/122845 */

int f(signed char a, int  b)
{
  int aa = a;
  int bb = b;
  aa &= bb;
  signed char t = aa;
  aa = t;
  aa &= bb;
  t = aa;
  return t;
}

/* There should be only 1 bit_and_expr left as both ands are the same. */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 1 "optimized"  } } */
