/* PR target/89434 */

#if __SIZEOF_INT__ == 4 && __SIZEOF_LONG_LONG__ == 8 && __CHAR_BIT__ == 8
long g = 0;

static inline unsigned long long
foo (unsigned long long u)
{
  unsigned x;
  __builtin_mul_overflow (-1, g, &x);
  u |= (unsigned) u < (unsigned short) x;
  return x - u;
}

int
main ()
{
  unsigned long long x = foo (0x222222222ULL);
  if (x != 0xfffffffddddddddeULL)
    __builtin_abort ();
  return 0;
}
#else
int
main ()
{
  return 0;
}
#endif
