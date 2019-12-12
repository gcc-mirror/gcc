/* { dg-do run } */

typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;

u32 a, b, c, d;

u32 foo (u32 f, u32 g, u32 g2, u32 g3, u16 h, u16 i)
{
  (void)g, (void)g2, (void)g3, (void)h;
  d = __builtin_bswap64 (i);
  __builtin_sub_overflow (0, d, &b);
  __builtin_memset (&i, c, 2);
  a = 0;
  return b + f + i + c;
}

int main (void)
{
  u32 x = foo (0, 0, 0, 0, 0, 0);
  asm ("" :: "r" (x));
  return 0;
}
