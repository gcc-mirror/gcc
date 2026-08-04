/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

/* It is incorrect to simplify these comparisons to true (or false),
   even with -Ofast.  */

int f (int i)
{
  return (float)i < 2147483648.0f; /* = 2^31 (INT_MAX + 1) */
}

int g (unsigned int i)
{
  return (float)i < 4294967296.0f; /* = 2^32 (UINT_MAX + 1) */
}

/* { dg-final { scan-tree-dump-not "return 0" "optimized" } } */
/* { dg-final { scan-tree-dump-not "return 1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "e\\+9" 2 "optimized" } } */
