/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#include "fx32.h"

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

MK_TEST (k)
MK_TEST (uk)

NI void test_div (float a, float b)
{
  test_div_k (a, b);
  test_div_uk (a, b);
}

// Results / args must be representable as float, so no rounding occurs.
// Non-overflow results must be representable as fixed, so no rounding occurs.
const PROGMEM float fvals[] =
  {
    0.0,
    +1.0, +2.0, +0.5, +0x1p5, +0x1p6, +0x1p-7,
    -1.0, -2.0, -0.5, -0x1p5, -0x1p6, -0x1p-7,
  };

NI void test1 (void)
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
  test1 ();
  return 0;
}
