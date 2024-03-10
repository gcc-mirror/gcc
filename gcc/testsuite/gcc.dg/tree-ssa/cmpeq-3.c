/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/109959 */


_Bool f2(unsigned a, int t)
{
  void g(void);
  if (t)
    return 0;
  g();
  if (a > 1)
    return 0;
  return a == 1;
}

/* These all should be optimized to `a == 1` */
/* { dg-final { scan-tree-dump-times "eq_expr," 1 "optimized"} } */
/* { dg-final { scan-tree-dump-not "le_expr," "optimized"} } */
/* { dg-final { scan-tree-dump-not "bit_and_expr," "optimized"} } */


