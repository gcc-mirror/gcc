/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

int test_combine(unsigned int a, unsigned int b)
{
  return __builtin_popcount(a&8) + __builtin_popcount(b&2);
}

/* { dg-final { scan-tree-dump-times "popcount" 1 "cddce1" } } */

