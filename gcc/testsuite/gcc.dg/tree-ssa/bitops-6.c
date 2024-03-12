/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* PR tree-optimization/111282 */


int f(int a, int b)
{
  return a & (b ^ ~a); // a & b
}

_Bool fb(_Bool x, _Bool y)
{
  return x & (y ^ !x); // x & y
}

int fa(int w, int z)
{
  return (~w) & (w ^ z); // ~w & z
}

int fcmp(int x, int y)
{
  _Bool a = x == 2;
  _Bool b = y == 1;
  return a & (b ^ !a); // (x == 2) & (y == 1)
}

/* { dg-final { scan-tree-dump-not   "bit_xor_expr, "   "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_not_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not   "ne_expr, "        "optimized" } } */
/* { dg-final { scan-tree-dump-times "eq_expr, "      2 "optimized" } } */

