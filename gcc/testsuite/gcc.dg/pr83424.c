/* PR rtl-optimization/83424 */
/* { dg-do run { target int128 } } */
/* { dg-options "-O -fno-tree-ccp -fno-tree-coalesce-vars" } */

typedef unsigned char u8;
typedef unsigned int u32;
typedef unsigned __int128 u128;

u32 a, c;
u8 b;

static u128 __attribute__ ((noinline, noclone))
foo (u128 p)
{
  u8 x = ~b;
  p &= c;
  x *= -p;
  x &= a == 0;
  x >>= 1;
  return p + x;
}

int
main (void)
{
  u128 x = foo (0);
  if (x != 0)
    __builtin_abort ();
  return 0;
}
