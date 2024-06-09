/* PR tree-optimization/109702 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Test to make sure unrelated arguments and comparisons
   don't get optimized incorrectly. */

__UINT16_TYPE__ test_bswap16(__UINT16_TYPE__ x, __UINT16_TYPE__ y)
{
  return x ? __builtin_bswap16(y) : 0;
}

__UINT32_TYPE__ test_bswap32(__UINT32_TYPE__ x, __UINT32_TYPE__ y)
{
  return x ? __builtin_bswap32(y) : 0;
}

__UINT64_TYPE__ test_bswap64(__UINT64_TYPE__ x, __UINT64_TYPE__ y)
{
  return x ? __builtin_bswap64(y) : 0;
}

int test_clrsb(int x, int y)
{
  return x ? __builtin_clrsb(y) : (__SIZEOF_INT__*8-1);
}

int test_clrsbl(long x, long y)
{
  return x ? __builtin_clrsbl(y) : (__SIZEOF_LONG__*8-1);
}

int test_clrsbll(long long x, long long y)
{
  return x ? __builtin_clrsbll(y) : (__SIZEOF_LONG_LONG__*8-1);
}

int test_parity(unsigned int x, unsigned int y)
{
  return x ? __builtin_parity(y) : 0;
}

int test_parityl(unsigned long x, unsigned long y)
{
  return x ? __builtin_parityl(y) : 0;
}

int test_parityll(unsigned long long x, unsigned long long y)
{
  return x ? __builtin_parityll(y) : 0;
}

int test_popcount(unsigned int x, unsigned int y)
{
  return x ? __builtin_popcount(y) : 0;
}

int test_popcountl(unsigned long x, unsigned long y)
{
  return x ? __builtin_popcountl(y) : 0;
}

int test_popcountll(unsigned long long x, unsigned long long y)
{
  return x ? __builtin_popcountll(y) : 0;
}

/* 4 types of functions, each with 3 types and there are 2 goto each */
/* { dg-final { scan-tree-dump-times "goto " 24 "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_..D. != 0" 12 "optimized" } } */
