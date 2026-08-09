/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

_Bool
a_ne_b_bit_ior (int a, int b)
{
  _Bool ret = ((a | b) != 0);
  return (((a != b) | ret) == ret);
}

_Bool
a_eq_b_bit_and (int a, int b)
{
  _Bool ret = ((a | b) == 0);
  return (((a == b) & ret) == ret);
}

/* { dg-final { scan-tree-dump-times "return 1;" 2 "optimized" } } */
