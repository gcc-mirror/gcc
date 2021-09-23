/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int test1(int x, int y)
{
#if __SIZEOF_INT__ == 4
  return __builtin_bswap32(x) == __builtin_bswap32(y);
#else
  return x == y;
#endif
}

int test2(int x, int y)
{
#if __SIZEOF_INT__ == 4
  return __builtin_bswap32(x) != __builtin_bswap32(y);
#else
  return x != y;
#endif
}

int test3(int x)
{
#if __SIZEOF_INT__ == 4
  return __builtin_bswap32(x) == 12345;
#else
  return x;
#endif
}

int test4(int x)
{
#if __SIZEOF_INT__ == 4
  return __builtin_bswap32(x) != 12345;
#else
  return x;
#endif
}

int test1ll(long long x, long long y)
{
#if __SIZEOF_LONG_LONG__ == 8
  return __builtin_bswap64(x) == __builtin_bswap64(y);
#else
  return x == y;
#endif
}

int test2ll(long long x, long long y)
{
#if __SIZEOF_LONG_LONG__ == 8
  return __builtin_bswap64(x) != __builtin_bswap64(y);
#else
  return x != y;
#endif
}

int test3ll(long long x)
{
#if __SIZEOF_LONG_LONG__ == 8
  return __builtin_bswap64(x) == 12345;
#else
  return (int)x;
#endif
}

int test4ll(long long x)
{
#if __SIZEOF_LONG_LONG__ == 8
  return __builtin_bswap64(x) != 12345;
#else
  return (int)x;
#endif
}

int test1s(short x, short y)
{
#if __SIZEOF_SHORT__ == 2
  return __builtin_bswap16(x) == __builtin_bswap16(y);
#else
  return x == y;
#endif
}

int test2s(short x, short y)
{
#if __SIZEOF_SHORT__ == 2
  return __builtin_bswap16(x) != __builtin_bswap16(y);
#else
  return x != y;
#endif
}

int test3s(short x)
{
#if __SIZEOF_SHORT__ == 2
  return __builtin_bswap16(x) == 12345;
#else
  return (int)x;
#endif
}

int test4s(short x)
{
#if __SIZEOF_SHORT__ == 2
  return __builtin_bswap16(x) != 12345;
#else
  return (int)x;
#endif
}

/* { dg-final { scan-tree-dump-times "__builtin_bswap" 0 "optimized" } } */

