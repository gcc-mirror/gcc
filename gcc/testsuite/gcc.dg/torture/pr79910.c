/* { dg-do run } */
/* { dg-additional-options "-fweb" } */

typedef unsigned char u8;
#if __SIZEOF_INT__ < 4
  __extension__ typedef __UINT32_TYPE__ u32;
#else
  typedef unsigned int u32;
#endif
typedef unsigned long long u64;
int a;

static __attribute__ ((noinline, noclone)) u64
foo (u8 p1, u32 p2)
{
  u64 b = a <= 0;
  p2 = 4;
  b >>= a == 0;
  p1 %= 0xfffffffff;
  p2 >>= b & 31;
  p1 += b;
  p2 <<= 31;
  return p1 + p2 + b;
}

int
main (void)
{
  u64 x = foo (0, 1);
  if (x != 0)
    __builtin_abort ();
  return 0;
}
