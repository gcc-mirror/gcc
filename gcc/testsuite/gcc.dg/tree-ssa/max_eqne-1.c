/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-raw" } */

/* PR tree-optimization/115275 */

unsigned maxne(unsigned a, unsigned b)
{
  unsigned t = a > b ? a : b;
  return t != 0;
}
unsigned maxeq(unsigned a, unsigned b)
{
  unsigned t = a > b ? a : b;
  return t == 0;
}
/* `max(a,b) == 0` should be optimized to `(a|b) == 0` for unsigned types. */
/* { dg-final { scan-tree-dump-not "max_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "eq_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "ne_expr, " 1 "optimized" } } */
