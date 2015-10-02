/* { dg-options "-O2 --save-temps" } */

#include "arm_neon.h"

int32x4_t
foo (int32x2_t *x)
{
  int32x2_t i = *x;
  int32x2_t zeroes = vcreate_s32 (0l);
  int32x4_t ret = vcombine_s32 (i, zeroes);
  return ret;
}

int32x4_t
bar (int64_t x)
{
  int32x2_t i = vcreate_s32 (x);
  int32x2_t zeroes = vcreate_s32 (0l);
  int32x4_t ret = vcombine_s32 (i, zeroes);
  return ret;
}

/* { dg-final { scan-assembler-not "mov\tv\[0-9\]+.8b, v\[0-9\]+.8b" } } */

