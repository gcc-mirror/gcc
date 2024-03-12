/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

int g(int x, int y)
{
  int xp = ~x;
  return (x | y) & (xp ^ y); // x & y
}
int g0(int x, int y)
{
  int xp = ~x;
  return (xp | y) & (x ^ y); // ~x & y
}

_Bool gb(_Bool x, _Bool y)
{
  _Bool xp = !x;
  return (x | y) & (xp ^ y); // x & y
}
_Bool gb0(_Bool x, _Bool y)
{
  _Bool xp = !x;
  return (xp | y) & (x ^ y); // !x & y
}


_Bool gbi(int a, int b)
{
  _Bool x = a < 2;
  _Bool y = b < 3;
  _Bool xp = !x;
  return (x | y) & (xp ^ y); // x & y
}
_Bool gbi0(int a, int b)
{
  _Bool x = a < 2;
  _Bool y = b < 3;
  _Bool xp = !x;
  return (xp | y) & (x ^ y); // !x & y
}

/* All of these should be optimized to `x & y` or `~x & y` */
/* { dg-final { scan-tree-dump-times "le_expr, " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "gt_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_xor_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr, " 6 "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_not_expr, " 2 "optimized" } } */
