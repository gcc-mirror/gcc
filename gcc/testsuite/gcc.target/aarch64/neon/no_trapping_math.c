/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */
/* { dg-additional-options "-fno-trapping-math" } */

/* These tests assert that floating point instructions are removed if the result
   is unused and `-ftrapping-math` is disabled. */

#include "arm_neon_test.h"

/*
** test_vadd_f16:
** ret
*/
void
test_vadd_f16 (float16x4_t a, float16x4_t b)
{
  vadd_f16 (a, b);
}

/*
** test_vadd_f32:
** ret
*/
void
test_vadd_f32 (float32x2_t a, float32x2_t b)
{
  vadd_f32 (a, b);
}

/*
** test_vadd_f64:
** ret
*/
void
test_vadd_f64 (float64x1_t a, float64x1_t b)
{
  vadd_f64 (a, b);
}

/*
** test_vaddq_f16:
** ret
*/
void
test_vaddq_f16 (float16x8_t a, float16x8_t b)
{
  vaddq_f16 (a, b);
}

/*
** test_vaddq_f32:
** ret
*/
void
test_vaddq_f32 (float32x4_t a, float32x4_t b)
{
  vaddq_f32 (a, b);
}

/*
** test_vaddq_f64:
** ret
*/
void
test_vaddq_f64 (float64x2_t a, float64x2_t b)
{
  vaddq_f64 (a, b);
}
