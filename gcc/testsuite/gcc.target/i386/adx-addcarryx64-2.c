/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-madx -O2" } */
/* { dg-require-effective-target adx } */

#include <x86intrin.h>
#include "adx-check.h"

static void
adx_test (void)
{
  volatile unsigned char c;
  unsigned long long x;
  volatile unsigned long long y, sum_ref;

  c = 0;
  x = y = 0xFFFFFFFFFFFFFFFFLL;
  sum_ref = 0xFFFFFFFFFFFFFFFELL;

  /* X = 0xFFFFFFFFFFFFFFFF, Y = 0xFFFFFFFFFFFFFFFF, C = 0.  */
  c = _addcarryx_u64 (c, x, y, &x);
  /* X = 0xFFFFFFFFFFFFFFFE, Y = 0xFFFFFFFFFFFFFFFF, C = 1.  */
  c = _addcarryx_u64 (c, x, y, &x);
  /* X = 0xFFFFFFFFFFFFFFFE, Y = 0xFFFFFFFFFFFFFFFF, C = 1.  */

  if (x != sum_ref)
    abort ();

  c = 0;
  x = y = 0xFFFFFFFFFFFFFFFFLL;
  sum_ref = 0xFFFFFFFFFFFFFFFELL;

  /* X = 0xFFFFFFFFFFFFFFFF, Y = 0xFFFFFFFFFFFFFFFF, C = 0.  */
  c = _addcarry_u64 (c, x, y, &x);
  /* X = 0xFFFFFFFFFFFFFFFE, Y = 0xFFFFFFFFFFFFFFFF, C = 1.  */
  c = _addcarry_u64 (c, x, y, &x);
  /* X = 0xFFFFFFFFFFFFFFFE, Y = 0xFFFFFFFFFFFFFFFF, C = 1.  */

  if (x != sum_ref)
    abort ();

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
    abort ();
}
