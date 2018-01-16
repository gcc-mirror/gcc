/* PR rtl-optimization/83393 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-forward-propagate -fno-tree-bit-ccp" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

u32 a, d;
u64 b;
u8 c;

static u64 __attribute__ ((noinline, noclone))
foo (u16 f, u64 g)
{
  f <<= 15;
  f *= d;
  f -= g %= 44;
  f <<= f <= g;
  c = 255;
  c >>= (u8) f == 0;
  f *= g;
  c -= ~c;
  return f + a + b + f;
}

int
main (void)
{
#if (__SIZEOF_LONG_LONG__ == 8 && __SIZEOF_INT__ == 4 \
     && __SIZEOF_SHORT__ == 2 && __CHAR_BIT__ == 8)
  u64 x = foo (3, 0xE6C0011BBA6DBD7LL);
  if (x != 0x1f66e)
    __builtin_abort ();
#endif
  return 0;
}
