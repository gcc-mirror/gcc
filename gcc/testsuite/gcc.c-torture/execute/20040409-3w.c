/* { dg-additional-options "-fwrapv" } */

#include <limits.h>

extern void abort ();

int test2(int x)
{
  return ~(x + INT_MIN);
}

int test3(int x)
{
  return ~(x - INT_MIN);
}

int test5(int x)
{
  int y = INT_MIN;
  return ~(x + y);
}

int test6(int x)
{
  int y = INT_MIN;
  return ~(x - y);
}


void test(int a, int b)
{
  if (test2(a) != b)
    abort();
  if (test3(a) != b)
    abort();
  if (test5(a) != b)
    abort();
  if (test6(a) != b)
    abort();
}


int main()
{
#if INT_MAX == 2147483647
  test(0x00000000,0x7fffffff);
  test(0x80000000,0xffffffff);
  test(0x12345678,0x6dcba987);
  test(0x92345678,0xedcba987);
  test(0x7fffffff,0x00000000);
  test(0xffffffff,0x80000000);
#endif

#if INT_MAX == 32767
  test(0x0000,0x7fff);
  test(0x8000,0xffff);
  test(0x1234,0x6dcb);
  test(0x9234,0xedcb);
  test(0x7fff,0x0000);
  test(0xffff,0x8000);
#endif

  return 0;
}

