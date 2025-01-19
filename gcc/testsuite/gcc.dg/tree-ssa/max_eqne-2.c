/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */

/* PR tree-optimization/115275 */
signed maxne(signed a, signed b)
{
  unsigned t = a > b ? a : b;
  return t != 0;
}
signed maxeq(signed a, signed b)
{
  unsigned t = a > b ? a : b;
  return t == 0;
}
/* For signed types, `max(a,b) == 0` should not optimized to `(a|b) == 0`. */
/* { dg-final { scan-tree-dump-times "max_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_ior_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "eq_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "ne_expr, " 1 "optimized" } } */
