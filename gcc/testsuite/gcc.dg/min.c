/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static inline int
min (int a, int b)
{
  return a < b ? a : b;
}

int
test_00 (int a)
{
  return min (a, a + 8);
}

int
test_01 (int a)
{
  return min (a, a - 8);
}

/* { dg-final { scan-tree-dump-not "MIN_EXPR" "optimized" } } */
