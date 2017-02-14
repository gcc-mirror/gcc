/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static inline int
max (int a, int b)
{
  return a < b ? b : a;
}

int
test_00 (int a)
{
  return max (a, a + 8);
}

int
test_01 (int a)
{
  return max (a, a - 8);
}

/* { dg-final { scan-tree-dump-not "MAX_EXPR" "optimized" } } */
