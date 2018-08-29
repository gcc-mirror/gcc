/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

__attribute__ ((noinline)) int8_t
testFunction1 (int8_t a, int8_t b)
{
  volatile int8x16_t sub = vsubq_s8 (vdupq_n_s8 (a), vdupq_n_s8 (b));
  int8x16_t abs = vabsq_s8 (sub);
  return vgetq_lane_s8 (abs, 0);
}

__attribute__ ((noinline)) int8_t
testFunction2 (int8_t a, int8_t b)
{
  int8x16_t sub = vsubq_s8 (vdupq_n_s8 (a), vdupq_n_s8 (b));
  int8x16_t abs = vabsq_s8 (sub);
  return vgetq_lane_s8 (abs, 0);
}

int
main (void)
{
  if (testFunction1 (-100, 100) != testFunction2 (-100, 100))
    __builtin_abort ();

  return 0;
}
