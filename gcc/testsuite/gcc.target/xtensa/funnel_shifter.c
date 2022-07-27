/* { dg-do compile } */
/* { dg-options "-O2" } */

unsigned int test_0(const void *addr)
{
  unsigned int n = (unsigned int)addr;
  const unsigned int *a = (const unsigned int*)(n & ~3);
  n = (n & 3) * 8;
  return (a[0] >> n) | (a[1] << (32 - n));
}

unsigned int test_1(unsigned int a, unsigned int b)
{
  return (a >> 16) + (b << 16);
}

/* { dg-final { scan-assembler-times "src" 2 } } */
