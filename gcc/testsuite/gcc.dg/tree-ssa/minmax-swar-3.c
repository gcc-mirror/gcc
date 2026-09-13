/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1-raw -fdump-tree-optimized-raw" } */

/* MIN (X + Y, Y) - Y -> MIN (X, 0) and MAX (X + Y, Y) - Y -> MAX (X, 0).  */

int
clamp_lo (int x, int y)
{
  int s = x + y;
  return (s < y ? s : y) - y;
}

int
clamp_hi (int x, int y)
{
  int s = x + y;
  return (s > y ? s : y) - y;
}

int
clamp_lo_long (int x, int y)
{
  int s = x + y;
  int m;
  if (s < y)
    m = s;
  else
    m = y;
  return m - y;
}

/* The two written with ?: fold in forwprop1; the one written with if and else
   needs phiopt to make the select first.  */
/* { dg-final { scan-tree-dump-times "<min_expr," 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "<max_expr," 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-not "<plus_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-not "<minus_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-times "<min_expr," 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "<max_expr," 1 "optimized" } } */
