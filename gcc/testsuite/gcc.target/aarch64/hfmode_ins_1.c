/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Check that we can perform this in a single INS without doing any DUPs.  */

#include <arm_neon.h>

float16x8_t
foo (float16x8_t a, float16x8_t b)
{
  return vsetq_lane_f16 (vgetq_lane_f16 (b, 2), a, 3);
}

float16x4_t
bar (float16x4_t a, float16x4_t b)
{
  return vset_lane_f16 (vget_lane_f16 (b, 2), a, 3);
}

/* { dg-final { scan-assembler-times "ins\\t" 2 } } */
/* { dg-final { scan-assembler-not "dup\\t" } } */
