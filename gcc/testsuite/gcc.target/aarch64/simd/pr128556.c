/* PR target/128556 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

__attribute__((noipa)) int8x8_t
f (int8x8_t acc, int8x8_t x, int8x8_t y)
{
  return vshr_n_s8 (vaba_s8 (acc, x, y), 1);
}

__attribute__((noipa)) int8x16_t
g (int8x16_t acc, int8x16_t x, int8x16_t y)
{
  return vshrq_n_s8 (vabaq_s8 (acc, x, y), 1);
}

int
main (void)
{
  int8_t acc[16] = { 100 };
  int8_t xs[16] = { 0 };
  int8_t ys[16] = { 100 };

  /* The accumulation wraps: (int8_t) (100 + 100) = -56.  */
  if (vget_lane_s8 (f (vld1_s8 (acc), vld1_s8 (xs), vld1_s8 (ys)), 0) != -28)
    __builtin_abort ();
  if (vgetq_lane_s8 (g (vld1q_s8 (acc), vld1q_s8 (xs), vld1q_s8 (ys)), 0)
      != -28)
    __builtin_abort ();
  return 0;
}
