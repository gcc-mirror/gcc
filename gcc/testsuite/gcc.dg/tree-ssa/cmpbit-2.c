/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-reassoc -fdump-tree-optimized-raw" } */

_Bool f(int a, int b)
{
  _Bool c = a == b;
  _Bool d = a > b;
  return c | d;
}

_Bool f1(int a, int b)
{
  _Bool c = a != b;
  _Bool d = a >= b;
  return c & d;
}

_Bool g(int a, int b)
{
  _Bool c = a == b;
  _Bool d = a < b;
  return c | d;
}

_Bool g1(int a, int b)
{
  _Bool c = a != b;
  _Bool d = a <= b;
  return c & d;
}


/* We should be able to optimize these without reassociation too. */
/* { dg-final { scan-tree-dump-not "bit_and_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-not "bit_ior_expr," "optimized" } } */
/* { dg-final { scan-tree-dump-times "gt_expr," 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "ge_expr," 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "lt_expr," 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "le_expr," 1 "optimized" } } */
