/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

// !!! Requires the fx64 <-> float conversions from AVR-LibC.

#include "fx64.h"

#define MK_TEST(fx)				    \
  NI bool in_range_##fx (float x)		    \
  {						    \
    return x < fmax_##fx && x >= fmin_##fx;	    \
  }						    \
						    \
  NI void test_div_##fx (float a, float b)	    \
  {						    \
    if (!in_range_##fx (a))			    \
      return;					    \
    if (!in_range_##fx (b))			    \
      return;					    \
    float f = b ? a / b : 0.0f;			    \
    __asm volatile ("" : "+r" (f));		    \
    fx##_t ax = (fx##_t) a;			    \
    fx##_t bx = (fx##_t) b;			    \
    fx##_t ab = ax / bx;			    \
    if ((b == 0 && a < 0) || (b && f < fmin_##fx))  \
      {						    \
	if (ab != min_##fx)			    \
	  exit (id_##fx + 1);			    \
	return;					    \
      }						    \
    if ((b == 0 && a >= 0) || (b && f > fmax_##fx)) \
      {						    \
	if (ab != max_##fx)			    \
	  exit (id_##fx + 2);			    \
	return;					    \
      }						    \
    if (f != (float) ab)			    \
      exit (id_##fx + 3);			    \
  }

MK_TEST (lk)
MK_TEST (ulk)
MK_TEST (llk)
MK_TEST (ullk)
MK_TEST (llr)
MK_TEST (ullr)

NI void test_div (float a, float b)
{
  test_div_lk (a, b);
  test_div_ulk (a, b);

  test_div_llk (a, b);
  test_div_ullk (a, b);

  test_div_llr (a, b);
  test_div_ullr (a, b);
}

// Results / args must be representable as float, so no rounding occurs.
// Non-overflow results must be representable as fixed, so no rounding occurs.
const PROGMEM float fvals[] =
  {
    0.0,
    +1.0, +2.0, +0.5, +0x1p10, +0x1p12, +0x1p-14,
    -1.0, -2.0, -0.5, -0x1p10, -0x1p12, -0x1p-14,
  };

NI void test (void)
{
  for (uint8_t a = 0; a < ARRAY_SIZE (fvals); ++a)
    for (uint8_t b = 0; b < ARRAY_SIZE (fvals); ++b)
      {
	float fa = pgm_read_float (&fvals[a]);
	float fb = pgm_read_float (&fvals[b]);
	test_div (fa, fb);
      }
}

int main (void)
{
  test ();
  return 0;
}
