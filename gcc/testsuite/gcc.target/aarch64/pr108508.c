/* { dg-options "-O3 -fharden-conditional-branches -fno-dce -fno-guess-branch-probability" } */

#include <arm_neon.h>

int
test_vld3q_lane_f64 (void)
{
  float64x2x3_t vectors;
  float64_t temp[2];
  int i, j;

  for (i = 0; i < 3; i++)
  {
    vst1q_f64 (temp, vectors.val[i]);
    for (j = 0; j < 2; j++)
      if (temp[j])
        return 1;
  }

  return 0;
}

void
foo (void)
{
  if (test_vld3q_lane_f64 () || test_vld3q_lane_f64 ())
    __builtin_abort ();
}
