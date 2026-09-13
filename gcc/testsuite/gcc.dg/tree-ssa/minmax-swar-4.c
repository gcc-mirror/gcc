/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized-raw" } */

/* Under -fwrapv the identities still hold where the range of the operands
   rules the wrap out.  */

int
swar_min_ranged (int a, int b)
{
  if (a < -1000 || a > 1000)
    __builtin_unreachable ();
  if (b < -1000 || b > 1000)
    __builtin_unreachable ();
  int t = a - b;
  return b + (t < 0 ? t : 0);
}

int
swar_max_ranged (int a, int b)
{
  if (a < -1000 || a > 1000)
    __builtin_unreachable ();
  if (b < -1000 || b > 1000)
    __builtin_unreachable ();
  int t = a - b;
  return a - (t < 0 ? t : 0);
}

int
clamp_lo_ranged (int x, int y)
{
  if (x < -1000 || x > 1000)
    __builtin_unreachable ();
  if (y < -1000 || y > 1000)
    __builtin_unreachable ();
  int s = x + y;
  return (s < y ? s : y) - y;
}

int
clamp_hi_ranged (int x, int y)
{
  if (x < -1000 || x > 1000)
    __builtin_unreachable ();
  if (y < -1000 || y > 1000)
    __builtin_unreachable ();
  int s = x + y;
  return (s > y ? s : y) - y;
}

/* { dg-final { scan-tree-dump-not "<plus_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-not "<minus_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-times "<min_expr," 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "<max_expr," 2 "optimized" } } */
