/* { dg-do run { target int128 } } */
/* { dg-options "-O -fno-tree-bit-ccp -fno-tree-reassoc" } */

typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef unsigned __int128 u128;

void __attribute__((noinline, noclone))
dirtify_stack(void)
{
  volatile char a[] = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
}

u128 __attribute__ ((noinline, noclone))
foo(u32 u32_1, u64 u64_1, u128 u128_1, u8 u8_2, u16 u16_2, u32 u32_2, u8 u8_3)
{
  u128_1 /= ~u128_1 | 1;
  u8_3 = ((u8_3 << 2) >> 1) << 7;
  u32_2 >>= u8_3;
  return u128_1 + u32_2 + u8_3;
}

int
main ()
{
  dirtify_stack();
  u128 x = foo(1, 1, 1, 1, 1, 1, 0);
  if (x != 1)
    __builtin_abort();
  return 0;
}
