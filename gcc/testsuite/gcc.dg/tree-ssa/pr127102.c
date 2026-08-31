/* PR tree-optimization/127102 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned
f1 (unsigned l1, unsigned c)
{
  if (c == 0)
    __builtin_unreachable ();
  unsigned m = l1 > c ? c : l1;
  return m != 0;
}

unsigned
f2 (unsigned l1, unsigned c)
{
  if (c == 0)
    __builtin_unreachable ();
  unsigned m = l1 > c ? c : l1;
  return m == 0;
}

int
f3 (int x, int y)
{
  if (y <= 0)
    __builtin_unreachable ();
  int m = x > y ? y : x;
  return m != 0;
}

int
f4 (int x, int y)
{
  if (y <= 0)
    __builtin_unreachable ();
  int m = x > y ? y : x;
  return m == 0;
}

/* { dg-final { scan-tree-dump-not "MIN_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-times " != 0" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times " == 0" 2 "optimized" } } */
