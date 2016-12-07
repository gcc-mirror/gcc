/* PR rtl-optimization/78546 */
/* { dg-do run { target int128 } } */

typedef unsigned __int128 u128;
u128 b;

static inline u128
foo (u128 p1)
{
  p1 += ~b;
  return -p1;
}

int
main ()
{
  asm volatile ("" : : : "memory");
  u128 x = foo (~0x7fffffffffffffffLL);
  if (x != 0x8000000000000001ULL)
    __builtin_abort ();
  return 0;
}
