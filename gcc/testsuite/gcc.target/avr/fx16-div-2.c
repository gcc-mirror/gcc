/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#include "fx16.h"

NI void test_hk (uint16_t a, float fb, uint16_t res)
{
  hk_t b = (hk_t) fb;
  if (hkbits (a) / b != hkbits (res))
    exit (id_hk + 4);
}

NI void test_uhk (uint16_t a, float fb, uint16_t res)
{
  uhk_t b = (hk_t) fb;
  if (uhkbits (a) / b != uhkbits (res))
    exit (id_uhk + 4);
}

NI void test2_uhk (uint16_t a, uint16_t b, uint16_t res)
{
  if (uhkbits (a) / uhkbits (b) != uhkbits (res))
    exit (id_uhk + 5);
}

NI void test2_hk (uint16_t a, uint16_t b, uint16_t res)
{
  if (hkbits (a) / hkbits (b) != hkbits (res))
    exit (id_hk + 5);
}

void test2 (void)
{
  test_uhk (+0xaabb, +0x11.0p0f, +0x0a0b);
  test_hk  (-0x77bb, +0x11.0p0f, -0x070b);
  test_hk  (+0x77bb, -0x11.0p0f, -0x070b);
  test_hk  (-0x77bb, -0x11.0p0f, +0x070b);
  test_hk  (-0x7ffe, 2.0f, -0x3fff);
  test_uhk (+0x7ffe, 2.0f, +0x3fff);
  test_uhk (+0xa955, 2.0f, +0x54aa);
  test_hk  (+0x5955, 2.0f, +0x2caa);
  test_hk  (-0x5955, 2.0f, -0x2caa);
  test_uhk (+0xffff, 2.0f, +0x7fff);
  test_hk  (-0x7fff, 2.0f, -0x3fff);
  test_hk  (+0, 0.0f, SMAX);
  test_hk  (+1, 0.0f, SMAX);
  test_hk  (-1, 0.0f, SMIN);
  test_uhk (+0, 0.0f, UMAX);
  test_uhk (+1, 0.0f, UMAX);
  test_hk  (SMAX - 100, +0x0.f8p0f, SMAX);
  test_hk  (SMIN + 100, +0x0.f8p0f, SMIN);
  test_hk  (SMAX - 100, -0x0.f8p0f, SMIN);
  test_hk  (SMIN + 100, -0x0.f8p0f, SMAX);
  test_uhk (UMAX - 100, +0x0.f8p0f, UMAX);
  test_uhk (UMAX - 100, +0x0.f8p0f, UMAX);

  test2_uhk (X80 + 1, X80,  uhk_1);
  test2_uhk (X80, X80 + 1,  uhk_1 - 1);
  test2_uhk (UMAX - 1, UMAX, uhk_1 - 1);
}

int main (void)
{
  test2 ();
  return 0;
}
