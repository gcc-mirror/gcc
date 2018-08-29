/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test_eqzero(unsigned int a)
{
  return __builtin_popcount(a) == 0;
}

int test_eqzerol(unsigned long b)
{
  return __builtin_popcountl(b) == 0;
}

int test_eqzeroll(unsigned long long c)
{
  return __builtin_popcountll(c) == 0;
}

int test_nezero(unsigned int d)
{
  return __builtin_popcount(d) != 0;
}

int test_nezerol(unsigned long e)
{
  return __builtin_popcountl(e) != 0;
}

int test_nezeroll(unsigned long long f)
{
  return __builtin_popcountll(f) != 0;
}

/* { dg-final { scan-tree-dump-times "popcount" 0 "original" } } */

