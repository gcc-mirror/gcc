/* PR rtl-optimization/89965 */
/* { dg-do run } */
/* { dg-options "-O -mtune=nano-x2 -fcaller-saves -fexpensive-optimizations -fno-tree-dce -fno-tree-ter" } */
/* { dg-additional-options "-march=i386" { target ia32 } } */

int a;

__attribute__ ((noipa)) unsigned long long
foo (unsigned char c, unsigned d, unsigned e, unsigned long long f,
     unsigned char g, unsigned h, unsigned long long i)
{
  (void) d;
  unsigned short j = __builtin_mul_overflow_p (~0, h, c);
  e <<= e;
  i >>= 7;
  c *= i;
  i /= 12;
  a = __builtin_popcount (c);
  __builtin_add_overflow (e, a, &f);
  return c + f + g + j + h;
}

__attribute__ ((noipa)) void
bar (void)
{
  char buf[64];
  __builtin_memset (buf, 0x55, sizeof buf);
  asm volatile ("" : : "r" (&buf[0]) : "memory");
}

int
main (void)
{
  bar ();
  unsigned long long x = foo (2, 0, 0, 0, 0, 0, 0);
  if (x != 0)
    __builtin_abort ();
  return 0;
}
