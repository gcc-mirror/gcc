/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

/* PR tree-optimization/85605 */
/* Like ssa-ifcombine-ccmp-2.c but with conversion from unsigned to signed in the
   inner bb which should be able to move too. */

int t (int a, unsigned b)
{
  if (a > 0)
    goto L1;
  signed t = b;
  if (t > 0)
    goto L1;
  return 0;
L1:
  return 1;
}
/* { dg-final { scan-tree-dump "\|" "optimized" } } */
