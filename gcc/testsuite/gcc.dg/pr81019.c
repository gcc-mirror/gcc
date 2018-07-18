/* PR rtl-optimization/81019 */
/* { dg-do run } */
/* { dg-options "-O -fno-tree-ccp" } */

unsigned long long __attribute__((noinline, noclone))
foo (unsigned char a, unsigned short b, unsigned c, unsigned long long d,
     unsigned char e, unsigned short f, unsigned g, unsigned long long h)
{
  g = e;
  c &= 0 < d;
  b *= d;
  g ^= -1;
  g &= 1;
  c |= 1;
  a -= 0 < g;
  g >>= 1;
  f = b | (f >> b);
  return a + c + d + f + g + h;
}

int
main (void)
{
  if (foo (0, 0, 0, 0, 0, 0, 0, 0) != 0x100)
    __builtin_abort ();
  return 0;
}
