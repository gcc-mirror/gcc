/* { dg-do compile } */
/* { dg-require-effective-target stdint_types_mbig_endian } */
/* { dg-options "-O3 -fdump-tree-optimized -mbig-endian" } */

#include <arm_neon.h>

#define VARIANTS				\
VARIANT (uint8x8_t, uint8x16_t, u8)		\
VARIANT (uint16x4_t, uint16x8_t, u16)		\
VARIANT (uint32x2_t, uint32x4_t, u32)		\
VARIANT (uint64x1_t, uint64x2_t, u64)		\
VARIANT (int8x8_t, int8x16_t, s8)		\
VARIANT (int16x4_t, int16x8_t, s16)		\
VARIANT (int32x2_t, int32x4_t, s32)		\
VARIANT (int64x1_t, int64x2_t, s64)		\
VARIANT (float16x4_t, float16x8_t, f16)		\
VARIANT (float32x2_t, float32x4_t, f32)		\
VARIANT (float64x1_t, float64x2_t, f64)		\
VARIANT (bfloat16x4_t, bfloat16x8_t, bf16)

/* vget_low_* intrinsics should become BIT_FIELD_REF. */
#define VARIANT(TYPE64, TYPE128, SUFFIX)	\
TYPE64						\
test_vget_low_##SUFFIX (TYPE128 vec)		\
{						\
  return vget_low_##SUFFIX (vec);		\
}

VARIANTS

/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF <vec_\[0-9\]*\\\(D\\\), 64, 64>" 12 "optimized" } } */
