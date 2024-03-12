/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

long long foo(long long x, int y)
{
  long long mask = ~0ull << 32;
  long long t = x & mask;
  long long r = t | (unsigned int)y;
  return r;
}

/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "orq" } } */
