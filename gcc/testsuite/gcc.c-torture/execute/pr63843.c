/* PR rtl-optimization/63843 */

static inline __attribute__ ((always_inline))
unsigned short foo (unsigned short v)
{
  return (v << 8) | (v >> 8);
}

unsigned short __attribute__ ((noinline, noclone, hot))
bar (unsigned char *x)
{
  unsigned int a;
  unsigned short b;
  __builtin_memcpy (&a, &x[0], sizeof (a));
  a ^= 0x80808080U;
  __builtin_memcpy (&x[0], &a, sizeof (a));
  __builtin_memcpy (&b, &x[2], sizeof (b));
  return foo (b);
}

int
main ()
{
  unsigned char x[8] = { 0x01, 0x01, 0x01, 0x01 };
  if (__CHAR_BIT__ == 8
      && sizeof (short) == 2
      && sizeof (int) == 4
      && bar (x) != 0x8181U)
    __builtin_abort ();
  return 0;
}
