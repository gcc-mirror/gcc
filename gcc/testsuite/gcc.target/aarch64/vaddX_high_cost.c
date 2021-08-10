/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_ADDL(rettype, intype, ts, rs) \
  rettype test_vaddl_ ## ts (intype a, intype b, intype c) \
	{ \
		rettype t0 = vaddl_ ## ts (vget_high_ ## ts (a), \
					   vget_high_ ## ts (c)); \
		rettype t1 = vaddl_ ## ts (vget_high_ ## ts (b), \
					   vget_high_ ## ts (c)); \
		return vaddq ## _ ## rs (t0, t1); \
	}

TEST_ADDL (int16x8_t, int8x16_t, s8, s16)
TEST_ADDL (uint16x8_t, uint8x16_t, u8, u16)
TEST_ADDL (int32x4_t, int16x8_t, s16, s32)
TEST_ADDL (uint32x4_t, uint16x8_t, u16, u32)
TEST_ADDL (int64x2_t, int32x4_t, s32, s64)
TEST_ADDL (uint64x2_t, uint32x4_t, u32, u64)

#define TEST_ADDW(rettype, intype, intypel, ts, rs) \
  rettype test_vaddw_ ## ts (intype a, intype b, intypel c) \
	{ \
		rettype t0 = vaddw_ ## ts (a, vget_high_ ## ts (c)); \
		rettype t1 = vaddw_ ## ts (b, vget_high_ ## ts (c)); \
		return vaddq ## _ ## rs (t0, t1); \
	}

TEST_ADDW (int16x8_t, int16x8_t, int8x16_t, s8, s16)
TEST_ADDW (uint16x8_t, uint16x8_t, uint8x16_t, u8, u16)
TEST_ADDW (int32x4_t, int32x4_t, int16x8_t, s16, s32)
TEST_ADDW (uint32x4_t, uint32x4_t, uint16x8_t, u16, u32)
TEST_ADDW (int64x2_t, int64x2_t, int32x4_t, s32, s64)
TEST_ADDW (uint64x2_t, uint64x2_t, uint32x4_t, u32, u64)

/* { dg-final { scan-assembler-not "dup\\t" } } */
