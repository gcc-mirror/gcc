

/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"
#include "stdio.h"

extern void abort (void);

void
test_square_root_v2sf ()
{
  const float32_t pool[] = {4.0f, 9.0f};
  float32x2_t val;
  float32x2_t res;

  val = vld1_f32 (pool);
  res = vsqrt_f32 (val);

  if (vget_lane_f32 (res, 0) != 2.0f)
    abort ();
  if (vget_lane_f32 (res, 1) != 3.0f)
    abort ();
}

void
test_square_root_v4sf ()
{
  const float32_t pool[] = {4.0f, 9.0f, 16.0f, 25.0f};
  float32x4_t val;
  float32x4_t res;

  val = vld1q_f32 (pool);
  res = vsqrtq_f32 (val);

  if (vgetq_lane_f32 (res, 0) != 2.0f)
    abort ();
  if (vgetq_lane_f32 (res, 1) != 3.0f)
    abort ();
  if (vgetq_lane_f32 (res, 2) != 4.0f)
    abort ();
  if (vgetq_lane_f32 (res, 3) != 5.0f)
    abort ();
}

void
test_square_root_v2df ()
{
  const float64_t pool[] = {4.0, 9.0};
  float64x2_t val;
  float64x2_t res;

  val = vld1q_f64 (pool);
  res = vsqrtq_f64 (val);

  if (vgetq_lane_f64 (res, 0) != 2.0)
    abort ();

  if (vgetq_lane_f64 (res, 1) != 3.0)
    abort ();
}

int
main (void)
{
  test_square_root_v2sf ();
  test_square_root_v4sf ();
  test_square_root_v2df ();

  return 0;
}
