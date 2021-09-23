/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_SUBL(rettype, intype, ts, rs) \
  rettype test_vsubl_ ## ts (intype a, intype b, intype c) \
	{ \
		rettype t0 = vsubl_ ## ts (vget_high_ ## ts (a), \
					   vget_high_ ## ts (c)); \
		rettype t1 = vsubl_ ## ts (vget_high_ ## ts (b), \
					   vget_high_ ## ts (c)); \
		return vaddq ## _ ## rs (t0, t1); \
	}

TEST_SUBL (int16x8_t, int8x16_t, s8, s16)
TEST_SUBL (uint16x8_t, uint8x16_t, u8, u16)
TEST_SUBL (int32x4_t, int16x8_t, s16, s32)
TEST_SUBL (uint32x4_t, uint16x8_t, u16, u32)
TEST_SUBL (int64x2_t, int32x4_t, s32, s64)
TEST_SUBL (uint64x2_t, uint32x4_t, u32, u64)

#define TEST_SUBW(rettype, intype, intypel, ts, rs) \
  rettype test_vsubw_ ## ts (intype a, intype b, intypel c) \
	{ \
		rettype t0 = vsubw_ ## ts (a, vget_high_ ## ts (c)); \
		rettype t1 = vsubw_ ## ts (b, vget_high_ ## ts (c)); \
		return vaddq ## _ ## rs (t0, t1); \
	}

TEST_SUBW (int16x8_t, int16x8_t, int8x16_t, s8, s16)
TEST_SUBW (uint16x8_t, uint16x8_t, uint8x16_t, u8, u16)
TEST_SUBW (int32x4_t, int32x4_t, int16x8_t, s16, s32)
TEST_SUBW (uint32x4_t, uint32x4_t, uint16x8_t, u16, u32)
TEST_SUBW (int64x2_t, int64x2_t, int32x4_t, s32, s64)
TEST_SUBW (uint64x2_t, uint64x2_t, uint32x4_t, u32, u64)

/* { dg-final { scan-assembler-not "dup\\t" } } */
