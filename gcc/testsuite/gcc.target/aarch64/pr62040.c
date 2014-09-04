/* { dg-do compile } */
/* { dg-options "-g -Os" } */

#include "arm_neon.h"

extern void bar (int32x4_t);

void
foo ()
{
  int32x4x4_t rows;
  uint64x2x2_t row01;

  row01.val[0] = vreinterpretq_u64_s32 (rows.val[0]);
  row01.val[1] = vreinterpretq_u64_s32 (rows.val[1]);
  uint64x1_t row3l = vget_low_u64 (row01.val[0]);
  row01.val[0] = vcombine_u64 (vget_low_u64 (row01.val[1]), row3l);
  int32x4_t xxx = vreinterpretq_s32_u64 (row01.val[0]);
  int32x4_t out = vtrn1q_s32 (xxx, xxx);
  bar (out);
}
