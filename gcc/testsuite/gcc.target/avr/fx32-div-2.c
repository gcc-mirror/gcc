/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#include "fx32.h"

NI void test_k (uint32_t a, float fb, uint32_t res)
{
  k_t b = (k_t) fb;
  if (kbits (a) / b != kbits (res))
    exit (id_k + 4);
}

NI void test_uk (uint32_t a, float fb, uint32_t res)
{
  uk_t b = (uk_t) fb;
  if (ukbits (a) / b != ukbits (res))
    exit (id_uk + 4);
}

NI void test2_k (uint32_t a, uint32_t b, uint32_t res)
{
  if (kbits (a) / kbits (b) != kbits (res))
    exit (id_k + 5);
}

NI void test2_uk (uint32_t a, uint32_t b, uint32_t res)
{
  if (ukbits (a) / ukbits (b) != ukbits (res))
    exit (id_uk + 5);
}

NI void test2 (void)
{
  test_uk (+0xaabbcc99, +0x11.0p0f, +0x0a0b0c09);
  test_k  (-0x77bbcc99, +0x11.0p0f, -0x070b0c09);
  test_k  (+0x77bbcc99, -0x11.0p0f, -0x070b0c09);
  test_k  (-0x77bbcc99, -0x11.0p0f, +0x070b0c09);
  test_k  (-0x7ffe0268, 2.0f, -0x3fff0134);
  test_uk (+0x7ffe0268, 2.0f, +0x3fff0134);
  test_uk (+0xc42ecb55, 2.0f, +0x621765aa);
  test_k  (-0x642ecb55, 2.0f, -0x321765aa);
  test_uk (+0xffffffff, 2.0f, +0x7fffffff);
  test_k  (-0x7fffffff, 2.0f, -0x3fffffff);
  test_k  (+0, 0.0f, SMAX);
  test_k  (+1, 0.0f, SMAX);
  test_k  (-1, 0.0f, SMIN);
  test_uk (+0, 0.0f, UMAX);
  test_uk (+1, 0.0f, UMAX);
  test_k  (SMAX - 1000, +0x0.fffp0f, SMAX);
  test_k  (SMIN + 1000, +0x0.fffp0f, SMIN);
  test_k  (SMAX - 1000, -0x0.fffp0f, SMIN);
  test_k  (SMIN + 1000, -0x0.fffp0f, SMAX);
  test_uk (UMAX - 1000, +0x0.fffp0f, UMAX);
  test_uk (UMAX - 1000, +0x0.fffp0f, UMAX);

  test2_uk (X80 + 1, X80, uk_1);
  test2_uk (X80, X80 + 1, uk_1 - 1);
  test2_uk (UMAX - 1, UMAX, uk_1 - 1);
  test2_k  (SMAX, SMIN, -(k_1 - 1));
  test2_k  (SMIN, SMAX, -k_1);
  test2_k  (SMAX, SMAX, k_1);
  test2_k  (SMAX, SMAX, k_1);
  test2_uk (SMIN, SMIN, uk_1);
  test2_uk (1, 1, uk_1);
  test2_k  (1, 1, k_1);
}

int main (void)
{
  test2 ();
  return 0;
}
