/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Verify the GIMPLE-level fix for PR112533. */

typedef unsigned int u32;

static _Bool
is_even (u32 a)
{
  return a % 2 == 0;
}

_Bool
same_evenness (u32 a, u32 b)
{
  return is_even (a) == is_even (b);
}

_Bool
diff_evenness (u32 a, u32 b)
{
  return is_even (a) != is_even (b);
}

_Bool
same_evenness_cmp_zero (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) == (t2 == 0);
}

_Bool
diff_evenness_cmp_zero (u32 a, u32 b)
{
  u32 t1 = a & 1;
  u32 t2 = b & 1;
  return (t1 == 0) != (t2 == 0);
}

/* Verify all functions canonicalize to xor forms.  */
/* { dg-final { scan-tree-dump-times "\\^" 4 "optimized" } } */
