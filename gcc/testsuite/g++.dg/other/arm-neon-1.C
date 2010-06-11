/* Basic smoke test for arm_neon.h */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

float a[4];

void test(void)
{
  float32x2x2_t v;
  float32x2_t res;
  v = vld2_f32(a);
  res = vadd_f32(v.val[0], v.val[1]);
  vst1_f32(a, res);
}
