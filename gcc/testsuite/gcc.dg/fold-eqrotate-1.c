/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int test1(unsigned int x, unsigned int y)
{
#if __SIZEOF_INT__ == 4
  unsigned int r1 = (x << 16) | (x >> 16);
  unsigned int r2 = (y << 16) | (y >> 16);
  return r1 == r2;
#else
  return x == y;
#endif
}

int test2(unsigned int x)
{
#if __SIZEOF_INT__ == 4
  unsigned int r1 = (x << 16) | (x >> 16);
  return r1 == 12345;
#else
  return x == 12345;
#endif
}

int test3(unsigned int x)
{
#if __SIZEOF_INT__ == 4
  unsigned int r1 = (x << 16) | (x >> 16);
  return r1 == 0;
#else
  return x == 0;
#endif
}

int test4(unsigned int x)
{
#if __SIZEOF_INT__ == 4
  unsigned int r1 = (x << 16) | (x >> 16);
  return r1 == ~0;
#else
  return x == ~0;
#endif
}

/* { dg-final { scan-tree-dump-times "r>>" 0 "optimized" } } */

