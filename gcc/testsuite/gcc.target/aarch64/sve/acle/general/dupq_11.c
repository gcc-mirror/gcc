/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-optimized" } */

#include <arm_sve.h>
#include <arm_neon.h>

svint8_t f_s8(int8x16_t x)
{
  return svdupq_s8 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7],
		    x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15]);
}

svint16_t f_s16(int16x8_t x)
{
  return svdupq_s16 (x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]);
}

svint32_t f_s32(int32x4_t x)
{
  return svdupq_s32 (x[0], x[1], x[2], x[3]);
}

svint64_t f_s64(int64x2_t x)
{
  return svdupq_s64 (x[0], x[1]);
}

/* { dg-final { scan-tree-dump "VEC_PERM_EXPR" "optimized" } } */
/* { dg-final { scan-tree-dump-not "svdupq" "optimized" } } */

/* { dg-final { scan-assembler-times {\tdup\tz[0-9]+\.q, z[0-9]+\.q\[0\]\n} 4 } } */
