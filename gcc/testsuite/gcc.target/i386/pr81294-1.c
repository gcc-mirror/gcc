/* PR target/81294 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include <x86intrin.h>

int
main ()
{
  volatile unsigned char c;
  unsigned int x;
  volatile unsigned int y, sum_ref;

  c = 0;
  x = 1;
  y = 0;
  sum_ref = 0x0;

  /* X = 0x00000001, Y = 0x00000000, C = 0.  */
  c = _subborrow_u32 (c, y, x, &x);
  /* X = 0xFFFFFFFF, Y = 0x00000000, C = 1.  */
  c = _subborrow_u32 (c, y, x, &x);
  /* X = 0xFFFFFFFF, Y = 0xFFFFFFFF, C = 1.  */

  if (x != sum_ref)
    __builtin_abort ();

  return 0;
}
