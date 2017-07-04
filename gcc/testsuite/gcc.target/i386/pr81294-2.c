/* PR target/81294 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <x86intrin.h>

int main ()
{
  volatile unsigned char c;
  unsigned long long x;
  volatile unsigned long long y, sum_ref;

  c = 0;
  x = 1LL;
  y = 0LL;
  sum_ref = 0x0LL;

  /* X = 0x0000000000000001, Y = 0x0000000000000000, C = 0.  */
  c = _subborrow_u64 (c, y, x, &x);
  /* X = 0xFFFFFFFFFFFFFFFF, Y = 0x0000000000000000, C = 1.  */
  c = _subborrow_u64 (c, y, x, &x);
  /* X = 0x0000000000000000, Y = 0x0000000000000000, C = 1.  */

  if (x != sum_ref)
    __builtin_abort ();

  return 0;
}
