#include "arm_neon.h"

#define TEST_VEOR3(T, S) T \
test_veor3q_ ## S (T a, T b, T c) \
{ \
  return veor3q_ ## S (a, b, c); \
} \

#define TEST_VBCAX(T, S) T \
test_vbcaxq_ ## S (T a, T b, T c) \
{ \
  return vbcaxq_ ## S (a, b, c); \
} \


TEST_VEOR3 (uint8x16_t, u8)
TEST_VEOR3 (uint16x8_t, u16)
TEST_VEOR3 (uint32x4_t, u32)
TEST_VEOR3 (uint64x2_t, u64)
TEST_VEOR3 (int8x16_t, s8)
TEST_VEOR3 (int16x8_t, s16)
TEST_VEOR3 (int32x4_t, s32)
TEST_VEOR3 (int64x2_t, s64)

uint64x2_t
test_vrax1q_u64 (uint64x2_t a, uint64x2_t b)
{
  return vrax1q_u64 (a, b);
}

uint64x2_t
test_vxarq_u64 (uint64x2_t a, uint64x2_t b)
{
  return vxarq_u64 (a, b, 15);
}

TEST_VBCAX (uint8x16_t, u8)
TEST_VBCAX (uint16x8_t, u16)
TEST_VBCAX (uint32x4_t, u32)
TEST_VBCAX (uint64x2_t, u64)
TEST_VBCAX (int8x16_t, s8)
TEST_VBCAX (int16x8_t, s16)
TEST_VBCAX (int32x4_t, s32)
TEST_VBCAX (int64x2_t, s64)

