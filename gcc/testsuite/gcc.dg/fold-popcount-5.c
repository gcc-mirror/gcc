/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

int test_and4(unsigned int a)
{
  return __builtin_popcount(a&4);
}

int test_and4l(unsigned long b)
{
  return __builtin_popcountl(b&4);
}

int test_and4ll(unsigned long long c)
{
  return __builtin_popcountll(c&4);
}

int test_shift(unsigned int d)
{
  int bits = 8*sizeof(unsigned int)-1;
  return __builtin_popcount(d<<31);
}

int test_shiftl(unsigned long e)
{
  int bits = 8*sizeof(unsigned long)-1;
  return __builtin_popcountl(e<<bits);
}

int test_shiftll(unsigned long long f)
{
  int bits = 8*sizeof(unsigned long long)-1;
  return __builtin_popcountll(f<<bits);
}

/* { dg-final { scan-tree-dump-times "popcount" 0 "optimized" } } */

