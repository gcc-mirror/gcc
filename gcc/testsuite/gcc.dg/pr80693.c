/* { dg-do run } */
/* { dg-options "-O -fno-tree-coalesce-vars" } */
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned u32;
typedef unsigned long long u64;

static u64 __attribute__((noinline, noclone))
foo(u8 u8_0, u16 u16_0, u32 u32_0, u64 u64_0,  u16 u16_1)
{
  u16_1 += 0x1051;
  u16_1 &= 1;
  u8_0 <<= u32_0 & 7;
  u16_0 -= !u16_1;
  u16_1 >>= ((u16)-u8_0 != 0xff);
  return u8_0 + u16_0 + u64_0 + u16_1;
}

int
main (void)
{
  u64 x = foo(1, 1, 0xffff, 0, 1);
  if (x != 0x80)
    __builtin_abort();
  return 0;
}
