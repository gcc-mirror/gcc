/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

/* PR tree-optimization/85605 */
/* Like ssa-ifcombine-ccmp-1.c but with conversion from unsigned to signed in the
   inner bb which should be able to move too. */

int t (int a, unsigned b)
{
  if (a > 0)
  {
    signed t = b;
    if (t > 0)
      return 0;
  }
  return 1;
}
/* { dg-final { scan-tree-dump "\&" "optimized" } } */
