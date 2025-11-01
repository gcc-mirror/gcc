/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

#include <arm_neon.h>

#ifndef TEST_COMBINE_HIGH_LOW_1
#define TEST_COMBINE_HIGH_LOW_1(TYPE, SUFF)				\
  TYPE rev_##TYPE##_1 (TYPE x)						\
  {									\
    return vcombine_##SUFF (vget_high_##SUFF (x), vget_low_##SUFF (x)); \
  }
#endif

#ifndef TEST_COMBINE_HIGH_LOW_2
#define TEST_COMBINE_HIGH_LOW_2(TYPE, SUFF)				\
  TYPE rev_##TYPE##_2 (TYPE x, TYPE y)					\
  {									\
    return vcombine_##SUFF (vget_high_##SUFF (x), vget_low_##SUFF (y)); \
  }
#endif

TEST_COMBINE_HIGH_LOW_1 (int8x16_t, s8)
TEST_COMBINE_HIGH_LOW_1 (int16x8_t, s16)
TEST_COMBINE_HIGH_LOW_1 (int32x4_t, s32)
TEST_COMBINE_HIGH_LOW_1 (int64x2_t, s64)
TEST_COMBINE_HIGH_LOW_1 (uint8x16_t, u8)
TEST_COMBINE_HIGH_LOW_1 (uint16x8_t, u16)
TEST_COMBINE_HIGH_LOW_1 (uint32x4_t, u32)
TEST_COMBINE_HIGH_LOW_1 (uint64x2_t, u64)
TEST_COMBINE_HIGH_LOW_1 (float16x8_t, f16)
TEST_COMBINE_HIGH_LOW_1 (float32x4_t, f32)

TEST_COMBINE_HIGH_LOW_2 (int8x16_t, s8)
TEST_COMBINE_HIGH_LOW_2 (int16x8_t, s16)
TEST_COMBINE_HIGH_LOW_2 (int32x4_t, s32)
TEST_COMBINE_HIGH_LOW_2 (int64x2_t, s64)
TEST_COMBINE_HIGH_LOW_2 (uint8x16_t, u8)
TEST_COMBINE_HIGH_LOW_2 (uint16x8_t, u16)
TEST_COMBINE_HIGH_LOW_2 (uint32x4_t, u32)
TEST_COMBINE_HIGH_LOW_2 (uint64x2_t, u64)
TEST_COMBINE_HIGH_LOW_2 (float16x8_t, f16)
TEST_COMBINE_HIGH_LOW_2 (float32x4_t, f32)

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 20 "optimized" } } */
/* { dg-final { scan-assembler-times {ext\tv0.16b, v0.16b, v0.16b, #8} 10 } } */
/* { dg-final { scan-assembler-times {ext\tv0.16b, v0.16b, v1.16b, #8} 10 } } */
