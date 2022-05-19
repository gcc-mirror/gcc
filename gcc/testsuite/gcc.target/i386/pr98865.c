/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

#if __SIZEOF_INT__ == 4
unsigned int foo(unsigned int a, unsigned int b)
{
  return (a >> 31) * b;
}

int bar(int a, int b)
{
  return -(a >> 31) * b;
}

int baz(int a, int b)
{
  int c = a >> 31;
  int d = -c;
  return d * b;
}

unsigned int pin(int a, unsigned int b)
{
  unsigned int t = a & 1;
  return t * b;
}
#endif

#if __SIZEOF_LONG_LONG__ == 8
unsigned long long fool(unsigned long long a, unsigned long long b)
{
  return (a >> 63) * b;
}

long long barl (long long a, long long b)
{
  return -(a >> 63) * b;
}

long long bazl (long long a, long long b)
{
  long long c = a >> 63;
  long long d = -c;
  return d * b;
}

unsigned long long pinl(long long a, unsigned long long b)
{
  unsigned long long t = a & 1;
  return t * b;
}
#endif

/* { dg-final { scan-assembler-not "imul" } } */
