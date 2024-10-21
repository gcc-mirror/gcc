/* { dg-do compile { target { aarch64*-*-* } } } */

#include <arm_neon.h>

#define TEST_128(T, S)			      \
T test_vreinterpretq_##S##_mf8 (mfloat8x16_t a)\
{					      \
  return vreinterpretq_##S##_mf8 (a);	      \
}					      \
					      \
mfloat8x16_t test_vreinterpretq_mf8_##S (T a) \
{					      \
  return vreinterpretq_mf8_##S (a);	      \
}


#define TEST_BOTH(T1, T2, S)		      \
TEST_128(T2, S)				      \
T1 test_vreinterpret_##S##_mf8 (mfloat8x8_t a) \
{					      \
  return vreinterpret_##S##_mf8 (a);	      \
}					      \
					      \
mfloat8x8_t test_vreinterpret_mf8_##S (T1 a)  \
{					      \
  return vreinterpret_mf8_##S (a);	      \
}

TEST_BOTH(bfloat16x4_t, bfloat16x8_t, bf16)
TEST_BOTH(float16x4_t, float16x8_t, f16)
TEST_BOTH(float32x2_t, float32x4_t, f32)
TEST_BOTH(float64x1_t, float64x2_t, f64)
TEST_BOTH(poly8x8_t, poly8x16_t, p8)
TEST_BOTH(poly16x4_t, poly16x8_t, p16)
TEST_BOTH(poly64x1_t, poly64x2_t, p64)
TEST_128(poly128_t, p128)
TEST_BOTH(int8x8_t, int8x16_t, s8)
TEST_BOTH(int16x4_t, int16x8_t, s16)
TEST_BOTH(int32x2_t, int32x4_t, s32)
TEST_BOTH(int64x1_t, int64x2_t, s64)
TEST_BOTH(uint8x8_t, uint8x16_t, u8)
TEST_BOTH(uint16x4_t, uint16x8_t, u16)
TEST_BOTH(uint32x2_t, uint32x4_t, u32)
TEST_BOTH(uint64x1_t, uint64x2_t, u64)


