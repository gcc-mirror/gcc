/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>
#include <math.h>
#include <stdlib.h>

int
test_frecps_float32_t (void)
{
  int i;
  float32_t value = 0.2;
  float32_t reciprocal = 5.0;
  float32_t step = vrecpes_f32 (value);
  /* 3 steps should give us within ~0.001 accuracy.  */
  for (i = 0; i < 3; i++)
    step = step * vrecpss_f32 (step, value);

  return fabs (step - reciprocal) < 0.001;
}

/* { dg-final { scan-assembler "frecpe\\ts\[0-9\]+, s\[0-9\]+" } } */
/* { dg-final { scan-assembler "frecps\\ts\[0-9\]+, s\[0-9\]+, s\[0-9\]+" } } */

int
test_frecps_float32x2_t (void)
{
  int i;
  int ret = 1;

  const float32_t value_pool[] = {0.2, 0.4};
  const float32_t reciprocal_pool[] = {5.0, 2.5};
  float32x2_t value = vld1_f32 (value_pool);
  float32x2_t reciprocal = vld1_f32 (reciprocal_pool);

  float32x2_t step = vrecpe_f32 (value);
  /* 3 steps should give us within ~0.001 accuracy.  */
  for (i = 0; i < 3; i++)
    step = step * vrecps_f32 (step, value);

  ret &= fabs (vget_lane_f32 (step, 0)
	       - vget_lane_f32 (reciprocal, 0)) < 0.001;
  ret &= fabs (vget_lane_f32 (step, 1)
	       - vget_lane_f32 (reciprocal, 1)) < 0.001;

  return ret;
}

/* { dg-final { scan-assembler "frecpe\\tv\[0-9\]+.2s, v\[0-9\]+.2s" } } */
/* { dg-final { scan-assembler "frecps\\tv\[0-9\]+.2s, v\[0-9\]+.2s, v\[0-9\]+.2s" } } */

int
test_frecps_float32x4_t (void)
{
  int i;
  int ret = 1;

  const float32_t value_pool[] = {0.2, 0.4, 0.5, 0.8};
  const float32_t reciprocal_pool[] = {5.0, 2.5, 2.0, 1.25};
  float32x4_t value = vld1q_f32 (value_pool);
  float32x4_t reciprocal = vld1q_f32 (reciprocal_pool);

  float32x4_t step = vrecpeq_f32 (value);
  /* 3 steps should give us within ~0.001 accuracy.  */
  for (i = 0; i < 3; i++)
    step = step * vrecpsq_f32 (step, value);

  ret &= fabs (vgetq_lane_f32 (step, 0)
	       - vgetq_lane_f32 (reciprocal, 0)) < 0.001;
  ret &= fabs (vgetq_lane_f32 (step, 1)
	       - vgetq_lane_f32 (reciprocal, 1)) < 0.001;
  ret &= fabs (vgetq_lane_f32 (step, 2)
	       - vgetq_lane_f32 (reciprocal, 2)) < 0.001;
  ret &= fabs (vgetq_lane_f32 (step, 3)
	       - vgetq_lane_f32 (reciprocal, 3)) < 0.001;

  return ret;
}

/* { dg-final { scan-assembler "frecpe\\tv\[0-9\]+.4s, v\[0-9\]+.4s" } } */
/* { dg-final { scan-assembler "frecps\\tv\[0-9\]+.4s, v\[0-9\]+.4s, v\[0-9\]+.4s" } } */

int
test_frecps_float64_t (void)
{
  int i;
  float64_t value = 0.2;
  float64_t reciprocal = 5.0;
  float64_t step = vrecped_f64 (value);
  /* 3 steps should give us within ~0.001 accuracy.  */
  for (i = 0; i < 3; i++)
    step = step * vrecpsd_f64 (step, value);

  return fabs (step - reciprocal) < 0.001;
}

/* { dg-final { scan-assembler "frecpe\\td\[0-9\]+, d\[0-9\]+" } } */
/* { dg-final { scan-assembler "frecps\\td\[0-9\]+, d\[0-9\]+, d\[0-9\]+" } } */

int
test_frecps_float64x2_t (void)
{
  int i;
  int ret = 1;

  const float64_t value_pool[] = {0.2, 0.4};
  const float64_t reciprocal_pool[] = {5.0, 2.5};
  float64x2_t value = vld1q_f64 (value_pool);
  float64x2_t reciprocal = vld1q_f64 (reciprocal_pool);

  float64x2_t step = vrecpeq_f64 (value);
  /* 3 steps should give us within ~0.001 accuracy.  */
  for (i = 0; i < 3; i++)
    step = step * vrecpsq_f64 (step, value);

  ret &= fabs (vgetq_lane_f64 (step, 0)
	       - vgetq_lane_f64 (reciprocal, 0)) < 0.001;
  ret &= fabs (vgetq_lane_f64 (step, 1)
	       - vgetq_lane_f64 (reciprocal, 1)) < 0.001;

  return ret;
}

/* { dg-final { scan-assembler "frecpe\\tv\[0-9\]+.2d, v\[0-9\]+.2d" } } */
/* { dg-final { scan-assembler "frecps\\tv\[0-9\]+.2d, v\[0-9\]+.2d, v\[0-9\]+.2d" } } */

int
main (int argc, char **argv)
{
  if (!test_frecps_float32_t ())
    abort ();
  if (!test_frecps_float32x2_t ())
    abort ();
  if (!test_frecps_float32x4_t ())
    abort ();
  if (!test_frecps_float64_t ())
    abort ();
  if (!test_frecps_float64x2_t ())
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
