/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/100798 */

int f(int a, int t)
{
  return (a=='s' ? ~t : t);
}

/* This should be convert into t^-(a=='s').  */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "negate_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_not_expr, " "optimized" } } */
