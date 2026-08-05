/* { dg-do run { target { ! avr_tiny } } } */
/* { dg-additional-options { -std=gnu99 -Os -mcall-prologues } } */

#include "fx32.h"

#define MK_TEST(fx)						\
  NI void test_div_##fx (uint32_t a, float fb, uint32_t r)	\
  {								\
    fx##_t ab = fx##bits (a) / (fx##_t) fb;			\
    if (ab != fx##bits (r))					\
      exit (id_##fx + 6);					\
    return;							\
  }

MK_TEST (k)
MK_TEST (uk)

NI void test1_div (uint32_t a, float fb, uint32_t r)
{
  test_div_uk (a, fb, r);
  if ((a & X80) == 0)
    {
      test_div_k (a, fb, r);
    }
}

NI void test2_div (uint32_t a, float fb, uint32_t r)
{
  test_div_uk (a, fb, r);
  if ((r & X80) == 0)
    {
      test_div_k (a, fb, r);
    }
  else
    {
      test_div_k (a, fb, SMAX);
    }
}

NI void test3 (void)
{
  for (uint32_t a = (uint32_t) 0xff01 << (32 - 16); a; a >>= 1)
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
