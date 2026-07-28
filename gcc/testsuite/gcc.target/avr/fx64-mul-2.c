/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

// !!! Requires the fx64 <-> float conversions from AVR-LibC.

#include "fx64.h"

NI void test_mul (float fa, float fb, uint64_t res)
{
  lk_t a = (lk_t) fa;
  lk_t b = (lk_t) fb;
  if (a * b != lkbits (res))
    exit (id_lk + 4);
}

void test (void)
{
  const float e16 = 0x1p-16f;
  const float e15 = 0x1p-15f;

  const float p0 = 0x1.0p+0f;
  const float p1 = 0x1.0p+1f;
  const float p16 = 0x1.0p+16f;
  const float p17 = 0x1.0p+17f;
  const float p31 = 0x1.0p+31f;
  const float p32 = 0x1.0p+32f;

  test_mul (-p16, -p16, SMAX);
  test_mul (+p16, +p16, SMAX);
  test_mul (+p16, -p16, SMIN);
  test_mul (-p16, +p16, SMIN);

  test_mul (-p16, -p17, SMAX);
  test_mul (+p16, +p17, SMAX);
  test_mul (+p16, -p17, SMIN);
  test_mul (-p16, +p17, SMIN);

  test_mul (-p17, -p16, SMAX);
  test_mul (+p17, +p16, SMAX);
  test_mul (+p17, -p16, SMIN);
  test_mul (-p17, +p16, SMIN);

  test_mul (+e16, +e15, 1);
  test_mul (-e16, -e15, 1);
  test_mul (-e16, +e15, -1ull);
  test_mul (+e16, -e15, -1ull);

  test_mul (+e16, +e16, 0);
  test_mul (-e16, +e16, 0);
  test_mul (-e16, -e16, 0);

  test_mul (-p32, -p0, SMAX);
  test_mul (-p32, +p0, SMIN);

  test_mul (-p31, -p1, SMAX);
  test_mul (-p31, +p1, SMIN);
  test_mul (+p31, +p1, SMAX);
  test_mul (+p31, -p1, SMIN);
}

int main (void)
{
  test ();
  return 0;
}
