/* PR rtl-optimization/128885 */
/* { dg-do run } */
/* { dg-require-effective-target int128 } */

typedef unsigned __int128 u128;

__attribute__((noipa)) u128
f (u128 b, unsigned long long m)
{
  return ((b >> 8) & (((u128) 1 << 57) - 1)) * m;
}

/* The mask covers bits that come from above the low 64 bits of B.  */
__attribute__((noipa)) u128
g (u128 b, unsigned long long m)
{
  return ((b >> 16) & (((u128) 1 << 50) - 1)) * m;
}

int
main (void)
{
  if (f (~(u128) 0, 1) != (((u128) 1 << 57) - 1))
    __builtin_abort ();
  if (g (~(u128) 0, 1) != (((u128) 1 << 50) - 1))
    __builtin_abort ();
  return 0;
}
