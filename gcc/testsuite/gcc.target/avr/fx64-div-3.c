/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

// !!! Requires the fx64 <-> float conversions from AVR-LibC.

#include "fx64.h"

#define MK_TEST(fx)						\
  NI void test_div_##fx (uint64_t a, float fb, uint64_t r)	\
  {								\
    fx##_t ab = fx##bits (a) / (fx##_t) fb;			\
    if (ab != fx##bits (r))					\
      exit (id_##fx + 6);					\
    return;							\
  }

MK_TEST (lk)
MK_TEST (ulk)
MK_TEST (llk)
MK_TEST (ullk)

NI void test1_div (uint64_t a, float fb, uint64_t r)
{
  test_div_ulk (a, fb, r);
  test_div_ullk (a, fb, r);
  if ((a & X80) == 0)
    {
      test_div_lk (a, fb, r);
      test_div_llk (a, fb, r);
    }
}

NI void test2_div (uint64_t a, float fb, uint64_t r)
{
  test_div_ulk (a, fb, r);
  test_div_ullk (a, fb, r);
  if ((r & X80) == 0)
    {
      test_div_lk (a, fb, r);
      test_div_llk (a, fb, r);
    }
  else
    {
      test_div_lk (a, fb, SMAX);
      test_div_llk (a, fb, SMAX);
    }
}

NI void test (void)
{
  for (uint64_t a = (uint64_t) 0xff01 << (64 - 16); a; a >>= 1)
    {
      test1_div (a, 2.0f, a >> 1);
      if ((a & X80) == 0)
	test2_div (a, 0.5f, a << 1);
    }
}

int main (void)
{
  test ();
  return 0;
}
