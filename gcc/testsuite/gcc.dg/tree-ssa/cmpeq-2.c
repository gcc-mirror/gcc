/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/109959 */

unsigned f(unsigned a)
{
  if (a <= 1)
    return a;
  return 0;
}

unsigned f0(unsigned a)
{
  if (a > 1)
    return 0;
  return a;
}

_Bool fb(unsigned a)
{
  if (a > 1)
    return 0;
  return a == 1;
}

/* These all should be optimized to `a == 1` */
/* { dg-final { scan-tree-dump-times "eq_expr," 3 "optimized"} } */
/* { dg-final { scan-tree-dump-not "le_expr," "optimized"} } */
/* { dg-final { scan-tree-dump-not "bit_and," "optimized"} } */
/* { dg-final { scan-tree-dump-not "gimple_phi " "optimized"} } */


