/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

// !!! Requires the fx64 <-> float conversions from AVR-LibC.

#include "fx64.h"

NI void test_lk (uint64_t a, float fb, uint64_t res)
{
  lk_t b = (lk_t) fb;
  if (lkbits (a) / b != lkbits (res))
    exit (id_lk + 4);
}

NI void test_ulk (uint64_t a, float fb, uint64_t res)
{
  ulk_t b = (ulk_t) fb;
  if (ulkbits (a) / b != ulkbits (res))
    exit (id_ulk + 4);
}

NI void test2_lk (uint64_t a, uint64_t b, uint64_t res)
{
  if (lkbits (a) / lkbits (b) != lkbits (res))
    exit (id_lk + 5);
}

NI void test2_ulk (uint64_t a, uint64_t b, uint64_t res)
{
  if (ulkbits (a) / ulkbits (b) != ulkbits (res))
    exit (id_ulk + 5);
}

NI void test (void)
{
  test_ulk (+0xaabbccddeeff7799, +0x11.0p0f, +0x0a0b0c0d0e0f0709);
  test_lk  (-0x77bbccddeeff7799, +0x11.0p0f, -0x070b0c0d0e0f0709);
  test_lk  (+0x77bbccddeeff7799, -0x11.0p0f, -0x070b0c0d0e0f0709);
  test_lk  (-0x77bbccddeeff7799, -0x11.0p0f, +0x070b0c0d0e0f0709);
  test_lk  (-0x7ffe02468ace2468, 2.0f, -0x3fff012345671234);
  test_ulk (+0x7ffe02468ace2468, 2.0f, +0x3fff012345671234);
  test_ulk (+0xc42eca8642eca955, 2.0f, +0x62176543217654aa);
  test_lk  (-0x642eca8642eca955, 2.0f, -0x32176543217654aa);
  test_ulk (+0xffffffffffffffff, 2.0f, +0x7fffffffffffffff);
  test_lk  (-0x7fffffffffffffff, 2.0f, -0x3fffffffffffffff);
  test_lk  (+0, 0.0f, SMAX);
  test_lk  (+1, 0.0f, SMAX);
  test_lk  (-1, 0.0f, SMIN);
  test_ulk (+0, 0.0f, UMAX);
  test_ulk (+1, 0.0f, UMAX);
  test_lk  (SMAX - 1000, +0x0.fffp0f, SMAX);
  test_lk  (SMIN + 1000, +0x0.fffp0f, SMIN);
  test_lk  (SMAX - 1000, -0x0.fffp0f, SMIN);
  test_lk  (SMIN + 1000, -0x0.fffp0f, SMAX);
  test_ulk (UMAX - 1000, +0x0.fffp0f, UMAX);
  test_ulk (UMAX - 1000, +0x0.fffp0f, UMAX);

  test2_ulk (X80 + 1, X80, ulk_1);
  test2_ulk (X80, X80 + 1, ulk_1 - 1);
  test2_ulk (UMAX - 1, UMAX, ulk_1 - 1);
  test2_lk  (SMAX, SMIN, -(lk_1 - 1));
  test2_lk  (SMIN, SMAX, -lk_1);
  test2_lk  (SMAX, SMAX, lk_1);
  test2_lk  (SMAX, SMAX, lk_1);
  test2_ulk (SMIN, SMIN, ulk_1);
  test2_ulk (1, 1, ulk_1);
  test2_lk  (1, 1, lk_1);
}

int main (void)
{
  test ();
  return 0;
}
