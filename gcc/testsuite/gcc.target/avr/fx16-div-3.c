/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#include "fx16.h"

#define MK_TEST(fx)						\
  NI void test_div_##fx (uint16_t a, float fb, uint16_t r)	\
  {								\
    if (fb < fmax_##fx)						\
      {								\
	fx##_t b = (fx##_t) fb;					\
	fx##_t ab = fx##bits (a) / b;				\
	if (ab != fx##bits (r))					\
	  exit (id_##fx + 6);					\
      }								\
  }

MK_TEST (hk)
MK_TEST (uhk)
MK_TEST (r)
MK_TEST (ur)

NI void test1_div (uint16_t a, float fb, uint16_t r)
{
  test_div_uhk (a, fb, r);
  test_div_ur (a, fb, r);
  if ((a & X80) == 0)
    {
      test_div_hk (a, fb, r);
      test_div_r (a, fb, r);
    }
}

NI void test2_div (uint16_t a, float fb, uint16_t r)
{
  test_div_uhk (a, fb, r);
  test_div_ur (a, fb, r);
  if ((r & X80) == 0)
    {
      test_div_hk (a, fb, r);
      test_div_r (a, fb, r);
    }
  else
    {
      test_div_hk (a, fb, SMAX);
      test_div_r (a, fb, SMAX);
    }
}

NI void test3 (void)
{
  for (uint16_t a = (uint16_t) 0xff01; a; a >>= 1)
    {
      test1_div (a, 2.0f, a >> 1);
      if ((a & X80) == 0)
	test2_div (a, 0.5f, a << 1);
    }
}

int main (void)
{
  test3 ();
  return 0;
}
