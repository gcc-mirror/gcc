/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

long long foo(long long x, int y)
{
  long long t = (long long)y << 32;
  return x ^ t;
}

long long bar(long long x, int y)
{
  long long t = (long long)y << 35;
  return x ^ t;
}

/* { dg-final { scan-assembler-times "xorl" 2 } } */
