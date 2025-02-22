/* { dg-do compile } */
/* { dg-options "-O2 -mlasx -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "WIDEN_MULT_EVEN_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump "WIDEN_MULT_ODD_EXPR" "optimized" } } */

typedef __INT32_TYPE__ i32;
typedef __INT64_TYPE__ i64;

i32 x[8], y[8];

i64
test (void)
{
  i64 ret = 0;
  for (int i = 0; i < 8; i++)
    ret ^= (i64) x[i] * y[i];
  return ret;
}
