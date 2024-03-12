/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo(unsigned int x)
{
#if __SIZEOF_INT__ == 4
  unsigned int y = (x>>4) | (x<<28);
  return __builtin_popcount(y);
#elif __SIZEOF_INT__ == 2
  unsigned int y = (x>>4) | (x<<12);
  return __builtin_popcount(y);
#else
  return x;
#endif
}

int bar(unsigned long x)
{
#if __SIZEOF_LONG__ == 8
  unsigned long y = (x>>4) | (x<<60);
  return __builtin_popcountl (y);
#elif __SIZEOF_LONG__ == 4
  unsigned long y = (x>>4) | (x<<28);
  return __builtin_popcountl (y);
#else
  return x;
#endif
}

int baz(unsigned long long x)
{
#if __SIZEOF_LONG_LONG__ == 8
  unsigned long long y = (x>>4) | (x<<60);
  return __builtin_popcountll (y);
#elif __SIZEOF_LONG_LONG__ == 4
  unsigned long long y = (x>>4) | (x<<28);
  return __builtin_popcountll (y);
#else
  return x;
#endif
}

/* { dg-final { scan-tree-dump-not " r>> " "optimized" } } */
