/* { dg-do run { target bitint } } */

#if __BITINT_MAXWIDTH__ >= 256
unsigned a, b, c, d, e;
unsigned _BitInt(256) f;

__attribute__((noipa)) unsigned short
bswap16 (int t)
{
  return __builtin_bswap16 (t);
}

void
foo (unsigned z, unsigned _BitInt(512) y, unsigned *r)
{
  unsigned t = __builtin_sub_overflow_p (0, y << 509, f);
  z *= bswap16 (t);
  d = __builtin_sub_overflow_p (c, 3, (unsigned _BitInt(512)) 0);
  unsigned q = z + c + b;
  unsigned short n = q >> (8 + a);
  *r = b + e + n;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 256
  unsigned x;
  foo (8, 2, &x);
  if (x != 8)
    __builtin_abort ();
#endif
  return 0;
}
