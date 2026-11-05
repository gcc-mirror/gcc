/* PR tree-optimization/127101 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Both functions should reduce to return 1.
   Chain of reasoning:
     a != 0  =>  a >= 1
     c != 0  =>  c >= 1
     l0 = l1 + a, l0 >= l1  =>  no unsigned overflow  =>  l0 >= a >= 1
     min(l0, c) != 0  [PR127102 pattern]  =>  l0 != 0  (c > 0)
     l0 >= 1  =>  l0 != 0  =>  1  */

unsigned
f (unsigned l1, unsigned a, unsigned c)
{
  if (a == 0)
    return 1;
  if (c == 0)
    return 1;
  unsigned l0 = l1 + a;
  if (l0 >= l1)
    {
      unsigned m = l0 > c ? c : l0;
      return m != 0;
    }
  return 1;
}

/* m is computed before the overflow check; range must be re-evaluated
   in the no-overflow branch context.  */
unsigned
f1 (unsigned l1, unsigned a, unsigned c)
{
  if (a == 0)
    return 1;
  if (c == 0)
    return 1;
  unsigned l0 = l1 + a;
  unsigned m = l0 > c ? c : l0;
  if (l0 >= l1)
    return m != 0;
  return 1;
}

/* { dg-final { scan-tree-dump-times "return 1" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "MIN_EXPR" "optimized" } } */
