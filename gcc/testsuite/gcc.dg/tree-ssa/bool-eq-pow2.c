/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

typedef unsigned int u32;

_Bool
eq_eq_outer_eq_mask2 (u32 a, u32 b)
{
  u32 t1 = a & 2;
  u32 t2 = b & 2;
  return (t1 == 0) == (t2 == 0);
}

_Bool
eq_eq_outer_eq_mask4 (u32 a, u32 b)
{
  u32 t1 = a & 4;
  u32 t2 = b & 4;
  return (t1 == 0) == (t2 == 0);
}

_Bool
eq_eq_outer_ne (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) != (t2 == 0);
}

_Bool
ne_eq_outer_eq (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 != 0) == (t2 == 0);
}

_Bool
ne_eq_outer_ne (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 != 0) != (t2 == 0);
}

_Bool
eq_ne_outer_eq (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) == (t2 != 0);
}

_Bool
eq_ne_outer_ne (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) != (t2 != 0);
}

_Bool
ne_ne_outer_eq (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 != 0) == (t2 != 0);
}

_Bool
ne_ne_outer_ne (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 != 0) != (t2 != 0);
}

/* Non-power-of-2: should not be simplified.  */
_Bool
eq_eq_outer_eq_nonpow2 (u32 a, u32 b)
{
  u32 t1 = a & 3;
  u32 t2 = b & 3;
  return (t1 == 0) == (t2 == 0);
}

/* Verify power-of-2 masks are simplified, non-power-of-2 masks are not.  */
/* { dg-final { scan-tree-dump-times "a_\[0-9\]+\\(D\\) \\^ b_\[0-9\]+\\(D\\)" 9 "optimized" } } */
/* { dg-final { scan-tree-dump-times "& 3" 2 "optimized" } } */
