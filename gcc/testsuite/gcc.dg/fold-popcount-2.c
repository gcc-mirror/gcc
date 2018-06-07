/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

int test_andone(unsigned int a)
{
  return __builtin_popcount(a&1);
}

int test_andonel(unsigned long b)
{
  return __builtin_popcountl(b&1);
}

int test_andonell(unsigned long long c)
{
  return __builtin_popcountll(c&1);
}

int test_oneand(unsigned int d)
{
  return __builtin_popcount(1&d);
}

int test_oneandl(unsigned long e)
{
  return __builtin_popcountl(1&e);
}

int test_oneandll(unsigned long long f)
{
  return __builtin_popcountll(1&f);
}

/* { dg-final { scan-tree-dump-times "popcount" 0 "cddce1" } } */

