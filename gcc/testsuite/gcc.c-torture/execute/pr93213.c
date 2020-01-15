/* PR tree-optimization/93213 - wrong code on a multibyte store with
   -Og -foptimize-strlen
   { dg-require-effective-target int128 }
   { dg-additional-options "-Og -foptimize-strlen" } */

typedef unsigned __INT16_TYPE__ u16;
typedef unsigned __INT32_TYPE__ u32;
typedef unsigned __int128 u128;

static inline u128
foo (u16 u16_1, u32 u32_1, u128 u128_1)
{
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  u128 u128_0 = 0;
  u128_1 -= __builtin_mul_overflow (u32_1, u16_1, &u32_1);
  __builtin_memmove (&u16_1, &u128_0, 2);
  __builtin_memmove (&u16_1, &u128_1, 1);
  return u16_1;
#else
  return 0xff;
#endif
}

__attribute__ ((noipa)) void
bar (void)
{
  char a[] = { 1, 2 };
  const char b[] = { 0, 0 };
  const char c[] = { 2 };
  __builtin_memcpy (a, b, 2);
  // The above is transformed into
  //   MEM <short unsigned int> [(char * {ref-all})&a] = 0;
  // which was then dropped because of the non-nul store below.
  __builtin_memcpy (a, c, 1);

  volatile char *p = a;
  if (p[0] != 2 || p[1] != 0)
    __builtin_abort ();
}

int
main (void)
{
  u16 x = foo (-1, -1, 0);
  if (x != 0xff)
    __builtin_abort ();

  bar ();
  return 0;
}
