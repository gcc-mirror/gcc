/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized --param logical-op-non-short-circuit=1" } */

/* PR tree-optimization/85605 */
/* Like ssa-ifcombine-ccmp-1.c but with conversion from short to int in the
   inner bb which should be able to move too. */

int t (int a, short b, int c)
{
  if (a > 0)
  {
    if (c == b)
      return 0;
  }
  return 1;
}
/* { dg-final { scan-tree-dump "\&" "optimized" } } */
