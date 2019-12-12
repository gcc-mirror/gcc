/* { dg-do compile } */
/* { dg-options "-O2 -funsafe-math-optimizations -fdump-tree-optimized-raw" } */

int
cmp_mul_1 (float x)
{
  return x * 3 <= 100;
}

int
cmp_mul_2 (float x)
{
  return x * -5 > 100;
}

int
div_cmp_1 (float x, float y)
{
  return x / 3 <= y;
}

int
div_cmp_2 (float x, float y)
{
  return x / 3 <= 1;
}

/* { dg-final { scan-tree-dump-times "mult_expr" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "rdiv_expr" "optimized" } } */
