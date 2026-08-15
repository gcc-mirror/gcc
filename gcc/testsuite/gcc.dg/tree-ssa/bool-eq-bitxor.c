/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

typedef unsigned int u32;

_Bool
xor_ne_eq (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 != 0) ^ (t2 == 0);
}

_Bool
xor_eq_eq (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) ^ (t2 == 0);
}

_Bool
xor_eq_ne (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) ^ (t2 != 0);
}

_Bool
xor_ne_ne (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 != 0) ^ (t2 != 0);
}

/* Verify all functions canonicalize to xor-mask tests.  */
/* { dg-final { scan-tree-dump-times "a_\[0-9\]+\\(D\\) \\^ b_\[0-9\]+\\(D\\)" 4 "optimized" } } */
