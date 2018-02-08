/* { dg-additional-options "-fwrapv" } */

#include <limits.h>

extern void abort ();

int test3(int x)
{
  return (x + INT_MIN) ^ 0x1234;
}

int test4(int x)
{
  return (x ^ 0x1234) + INT_MIN;
}

int test5(int x)
{
  return (x - INT_MIN) ^ 0x1234;
}

int test6(int x)
{
  return (x ^ 0x1234) - INT_MIN;
}

int test9(int x)
{
  int y = INT_MIN;
  int z = 0x1234;
  return (x + y) ^ z;
}

int test10(int x)
{
  int y = 0x1234;
  int z = INT_MIN;
  return (x ^ y) + z;
}

int test11(int x)
{
  int y = INT_MIN;
  int z = 0x1234;
  return (x - y) ^ z;
}

int test12(int x)
{
  int y = 0x1234;
  int z = INT_MIN;
  return (x ^ y) - z;
}


void test(int a, int b)
{
  if (test3(a) != b)
    abort();
  if (test4(a) != b)
    abort();
  if (test5(a) != b)
    abort();
  if (test6(a) != b)
    abort();
  if (test9(a) != b)
    abort();
  if (test10(a) != b)
    abort();
  if (test11(a) != b)
    abort();
  if (test12(a) != b)
    abort();
}


int main()
{
#if INT_MAX == 2147483647
  test(0x00000000,0x80001234);
  test(0x00001234,0x80000000);
  test(0x80000000,0x00001234);
  test(0x80001234,0x00000000);
  test(0x7fffffff,0xffffedcb);
  test(0xffffffff,0x7fffedcb);
#endif

#if INT_MAX == 32767
  test(0x0000,0x9234);
  test(0x1234,0x8000);
  test(0x8000,0x1234);
  test(0x9234,0x0000);
  test(0x7fff,0xedcb);
  test(0xffff,0x6dcb);
#endif

  return 0;
}

