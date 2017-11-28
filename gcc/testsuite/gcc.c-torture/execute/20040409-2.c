#include <limits.h>

extern void abort ();

int test1(int x)
{
  return (x ^ INT_MIN) ^ 0x1234;
}

unsigned int test1u(unsigned int x)
{
  return (x ^ (unsigned int)INT_MIN) ^ 0x1234;
}

int test2(int x)
{
  return (x ^ 0x1234) ^ INT_MIN;
}

unsigned int test2u(unsigned int x)
{
  return (x ^ 0x1234) ^ (unsigned int)INT_MIN;
}

unsigned int test3u(unsigned int x)
{
  return (x + (unsigned int)INT_MIN) ^ 0x1234;
}

unsigned int test4u(unsigned int x)
{
  return (x ^ 0x1234) + (unsigned int)INT_MIN;
}

unsigned int test5u(unsigned int x)
{
  return (x - (unsigned int)INT_MIN) ^ 0x1234;
}

unsigned int test6u(unsigned int x)
{
  return (x ^ 0x1234) - (unsigned int)INT_MIN;
}

int test7(int x)
{
  int y = INT_MIN;
  int z = 0x1234;
  return (x ^ y) ^ z;
}

unsigned int test7u(unsigned int x)
{
  unsigned int y = (unsigned int)INT_MIN;
  unsigned int z = 0x1234;
  return (x ^ y) ^ z;
}

int test8(int x)
{
  int y = 0x1234;
  int z = INT_MIN;
  return (x ^ y) ^ z;
}

unsigned int test8u(unsigned int x)
{
  unsigned int y = 0x1234;
  unsigned int z = (unsigned int)INT_MIN;
  return (x ^ y) ^ z;
}

unsigned int test9u(unsigned int x)
{
  unsigned int y = (unsigned int)INT_MIN;
  unsigned int z = 0x1234;
  return (x + y) ^ z;
}

unsigned int test10u(unsigned int x)
{
  unsigned int y = 0x1234;
  unsigned int z = (unsigned int)INT_MIN;
  return (x ^ y) + z;
}

unsigned int test11u(unsigned int x)
{
  unsigned int y = (unsigned int)INT_MIN;
  unsigned int z = 0x1234;
  return (x - y) ^ z;
}

unsigned int test12u(unsigned int x)
{
  unsigned int y = 0x1234;
  unsigned int z = (unsigned int)INT_MIN;
  return (x ^ y) - z;
}


void test(int a, int b)
{
  if (test1(a) != b)
    abort();
  if (test2(a) != b)
    abort();
  if (test7(a) != b)
    abort();
  if (test8(a) != b)
    abort();
}

void testu(unsigned int a, unsigned int b)
{
  if (test1u(a) != b)
    abort();
  if (test2u(a) != b)
    abort();
  if (test3u(a) != b)
    abort();
  if (test4u(a) != b)
    abort();
  if (test5u(a) != b)
    abort();
  if (test6u(a) != b)
    abort();
  if (test7u(a) != b)
    abort();
  if (test8u(a) != b)
    abort();
  if (test9u(a) != b)
    abort();
  if (test10u(a) != b)
    abort();
  if (test11u(a) != b)
    abort();
  if (test12u(a) != b)
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

  testu(0x00000000,0x80001234);
  testu(0x00001234,0x80000000);
  testu(0x80000000,0x00001234);
  testu(0x80001234,0x00000000);
  testu(0x7fffffff,0xffffedcb);
  testu(0xffffffff,0x7fffedcb);
#endif

#if INT_MAX == 32767
  test(0x0000,0x9234);
  test(0x1234,0x8000);
  test(0x8000,0x1234);
  test(0x9234,0x0000);
  test(0x7fff,0xedcb);
  test(0xffff,0x6dcb);

  testu(0x0000,0x9234);
  testu(0x8000,0x1234);
  testu(0x1234,0x8000);
  testu(0x9234,0x0000);
  testu(0x7fff,0xedcb);
  testu(0xffff,0x6dcb);
#endif

  return 0;
}

