/* { dg-do run } */
/* { dg-options "-madx -O2" } */
/* { dg-require-effective-target adx } */

#include <x86intrin.h>
#include "adx-check.h"

static void
adx_test (void)
{
  volatile unsigned char c;
  unsigned int x;
  volatile unsigned int y, sum_ref;

  c = 0;
  x = y = 0xFFFFFFFF;
  sum_ref = 0xFFFFFFFE;

  /* X = 0xFFFFFFFF, Y = 0xFFFFFFFF, C = 0.  */
  c = _addcarryx_u32 (c, x, y, &x);
  /* X = 0xFFFFFFFE, Y = 0xFFFFFFFF, C = 1.  */
  c = _addcarryx_u32 (c, x, y, &x);
  /* X = 0xFFFFFFFE, Y = 0xFFFFFFFF, C = 1.  */

  if (x != sum_ref)
    abort ();

  c = 0;
  x = y = 0xFFFFFFFF;
  sum_ref = 0xFFFFFFFE;

  /* X = 0xFFFFFFFF, Y = 0xFFFFFFFF, C = 0.  */
  c = _addcarry_u32 (c, x, y, &x);
  /* X = 0xFFFFFFFE, Y = 0xFFFFFFFF, C = 1.  */
  c = _addcarry_u32 (c, x, y, &x);
  /* X = 0xFFFFFFFE, Y = 0xFFFFFFFF, C = 1.  */

  if (x != sum_ref)
    abort ();

  c = 0;
  x = 1;
  y = 0;
  sum_ref = 0x0;

  /* X = 0x00000001, Y = 0x00000000, C = 0.  */
  c = _subborrow_u32 (c, x, y, &x);
  /* X = 0xFFFFFFFF, Y = 0x00000000, C = 1.  */
  c = _subborrow_u32 (c, x, y, &x);
  /* X = 0xFFFFFFFF, Y = 0xFFFFFFFF, C = 1.  */

  if (x != sum_ref)
    abort ();
}
