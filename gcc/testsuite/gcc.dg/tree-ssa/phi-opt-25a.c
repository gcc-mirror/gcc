/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

unsigned short test_bswap16(unsigned short x)
{
  if (x)
    return __builtin_bswap16(x);
  return 0;
}

unsigned int test_bswap32(unsigned int x)
{
  if (x)
    return __builtin_bswap32(x);
  return 0;
}

unsigned long long test_bswap64(unsigned long long x)
{
  if (x)
    return __builtin_bswap64(x);
  return 0;
}

int test_clrsb(int x)
{
  if (x)
    return __builtin_clrsb(x);
  return (__SIZEOF_INT__*8-1);
}

int test_clrsbl(long x)
{
  if (x)
    return __builtin_clrsbl(x);
  return (__SIZEOF_LONG__*8-1);
}

int test_clrsbll(long long x)
{
  if (x)
    return __builtin_clrsbll(x);
  return (__SIZEOF_LONG_LONG__*8-1);
}

int test_parity(int x)
{
  if (x)
    return __builtin_parity(x);
  return 0;
}

int test_parityl(long x)
{
  if (x)
    return __builtin_parityl(x);
  return 0;
}

int test_parityll(long long x)
{
  if (x)
    return __builtin_parityll(x);
  return 0;
}

int test_popcount(int x)
{
  if (x)
    return __builtin_popcount(x);
  return 0;
}

int test_popcountl(long x)
{
  if (x)
    return __builtin_popcountl(x);
  return 0;
}

int test_popcountll(long long x)
{
  if (x)
    return __builtin_popcountll(x);
  return 0;
}

/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */

