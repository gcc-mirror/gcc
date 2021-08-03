/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned short test_bswap16(unsigned short x)
{
  return x ? __builtin_bswap16(x) : 0;
}

unsigned int test_bswap32(unsigned int x)
{
  return x ? __builtin_bswap32(x) : 0;
}

unsigned long long test_bswap64(unsigned long long x)
{
  return x ? __builtin_bswap64(x) : 0;
}

int test_clrsb(int x)
{
  return x ? __builtin_clrsb(x) : (__SIZEOF_INT__*8-1);
}

int test_clrsbl(long x)
{
  return x ? __builtin_clrsbl(x) : (__SIZEOF_LONG__*8-1);
}

int test_clrsbll(long long x)
{
  return x ? __builtin_clrsbll(x) : (__SIZEOF_LONG_LONG__*8-1);
}

#if 0
/* BUILT_IN_FFS is transformed by match.pd */
int test_ffs(unsigned int x)
{
  return x ? __builtin_ffs(x) : 0;
}

int test_ffsl(unsigned long x)
{
  return x ? __builtin_ffsl(x) : 0;
}

int test_ffsll(unsigned long long x)
{
  return x ? __builtin_ffsll(x) : 0;
}
#endif

int test_parity(int x)
{
  return x ? __builtin_parity(x) : 0;
}

int test_parityl(long x)
{
  return x ? __builtin_parityl(x) : 0;
}

int test_parityll(long long x)
{
  return x ? __builtin_parityll(x) : 0;
}

int test_popcount(int x)
{
  return x ? __builtin_popcount(x) : 0;
}

int test_popcountl(long x)
{
  return x ? __builtin_popcountl(x) : 0;
}

int test_popcountll(long long x)
{
  return x ? __builtin_popcountll(x) : 0;
}

/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */

