/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(unsigned int x)
{
#if __SIZEOF_INT__ == 4
  return __builtin_popcount (__builtin_bswap32(x));
#elif __SIZEOF_INT__ == 2
  return __builtin_popcount (__builtin_bswap16(x));
#else
  return x;
#endif
}

int bar(unsigned long x)
{
#if __SIZEOF_LONG__ == 8
  return __builtin_popcountl (__builtin_bswap64(x));
#elif __SIZEOF_LONG__ == 4
  return __builtin_popcountl (__builtin_bswap32(x));
#else
  return x;
#endif
}

int baz(unsigned long long x)
{
#if __SIZEOF_LONG_LONG__ == 8
  return __builtin_popcountll (__builtin_bswap64(x));
#elif __SIZEOF_LONG_LONG__ == 4
  return __builtin_popcountll (__builtin_bswap32(x));
#else
  return x;
#endif
}

/* { dg-final { scan-tree-dump-not "bswap" "optimized" } } */
