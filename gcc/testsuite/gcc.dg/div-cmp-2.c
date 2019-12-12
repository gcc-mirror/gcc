/* { dg-do compile } */
/* { dg-options "-O2 -funsafe-math-optimizations -ffinite-math-only -fdump-tree-optimized-raw" } */

int
cmp_1 (float x)
{
  return 5 / x >= 0;
}

int
cmp_2 (float x)
{
  return 1 / x <= 0;
}

int
cmp_3 (float x)
{
  return -2 / x >= 0;
}

int
cmp_4 (float x)
{
  return -5 / x <= 0;
}

/* { dg-final { scan-tree-dump-not "rdiv_expr" "optimized" } } */
