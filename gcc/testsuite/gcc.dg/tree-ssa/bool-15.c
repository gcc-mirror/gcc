/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/110937 */

_Bool f2(int x, int y, int w, int z)
{
  _Bool a = x == y;
  _Bool b = w == z;
  if (a)
    return !b;
  return b;
}

/* We should be able to remove the conditional and convert it to an xor. */
/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_phi " "optimized" } } */
/* { dg-final { scan-tree-dump-not "ne_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 1 "optimized" } } */
