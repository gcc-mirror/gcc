/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O -fschedule-insns" } */

typedef unsigned char u8;
typedef unsigned __int128 u128;

static u128 u128_0;
static u128 *p128;

u128 __attribute__ ((noinline, noclone))
foo(u8 u8_0)
{
  p128 = &u128_0;
  u128_0 = u8_0;
  u128_0 = u128_0 << 127 | u128_0 >> 1;
  u128_0 >>= (u8)u128_0;
  return 2 + u128_0;
}

int
main()
{
  u128 x = foo(5);
  if (p128 != &u128_0)
    __builtin_abort();
  if (u128_0 != ((u128)2 << 124))
    __builtin_abort();
  if (x != ((u128)2 << 124) + 2)
    __builtin_abort();
  return 0;
}
