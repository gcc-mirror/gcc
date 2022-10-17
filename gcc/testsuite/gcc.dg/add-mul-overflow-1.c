/* { dg-options "-O -fdump-tree-optimized" } */

int res[4];

void
f1 (int x, int y)
{
  res[2] = __builtin_add_overflow (x, y, res + 0);
  res[3] = __builtin_add_overflow (y, x, res + 1);
}

void
f2 (int x, int y)
{
  res[2] = __builtin_sub_overflow (x, y, res + 0);
  res[3] = __builtin_sub_overflow (y, x, res + 1);
}

void
f3 (int x, int y)
{
  res[2] = __builtin_mul_overflow (x, y, res + 0);
  res[3] = __builtin_mul_overflow (y, x, res + 1);
}

/* { dg-final { scan-tree-dump-times {\.ADD_OVERFLOW} 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.SUB_OVERFLOW} 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times {\.MUL_OVERFLOW} 1 "optimized" } } */
