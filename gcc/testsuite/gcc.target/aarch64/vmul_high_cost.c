/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_MULL_VEC(name, rettype, intype, ts, rs) \
  rettype test_ ## name ## _ ## ts (intype a, intype b, intype c) \
	{ \
		rettype t0 = name ## _ ## ts (vget_high_ ## ts (a), \
					      vget_high_ ## ts (c)); \
		rettype t1 = name ## _ ## ts (vget_high_ ## ts (b), \
					      vget_high_ ## ts (c)); \
		return vqaddq ## _ ## rs (t0, t1); \
	}

TEST_MULL_VEC (vmull, int16x8_t, int8x16_t, s8, s16)
TEST_MULL_VEC (vmull, uint16x8_t, uint8x16_t, u8, u16)
TEST_MULL_VEC (vmull, int32x4_t, int16x8_t, s16, s32)
TEST_MULL_VEC (vmull, uint32x4_t, uint16x8_t, u16, u32)
TEST_MULL_VEC (vmull, int64x2_t, int32x4_t, s32, s64)
TEST_MULL_VEC (vmull, uint64x2_t, uint32x4_t, u32, u64)

TEST_MULL_VEC (vqdmull, int32x4_t, int16x8_t, s16, s32)
TEST_MULL_VEC (vqdmull, int64x2_t, int32x4_t, s32, s64)

#define TEST_MULL_N(name, rettype, intype, ts, rs) \
  rettype test_ ## name ## _ ## ts (intype a, intype b, intype c) \
	{ \
		rettype t0 = name ## _ ## ts (vget_high_ ## ts (a), b[1]); \
		rettype t1 = name ## _ ## ts (vget_high_ ## ts (a), c[1]); \
		return vqaddq ## _ ## rs (t0, t1); \
	}

TEST_MULL_N (vmull_n, int32x4_t, int16x8_t, s16, s32)
TEST_MULL_N (vmull_n, uint32x4_t, uint16x8_t, u16, u32)
TEST_MULL_N (vmull_n, int64x2_t, int32x4_t, s32, s64)
TEST_MULL_N (vmull_n, uint64x2_t, uint32x4_t, u32, u64)

TEST_MULL_N (vqdmull_n, int32x4_t, int16x8_t, s16, s32)
TEST_MULL_N (vqdmull_n, int64x2_t, int32x4_t, s32, s64)

#define TEST_MLXL_VEC(name, rettype, intype, ts) \
  rettype test_ ## name ## _ ## ts (rettype acc, intype a, intype b, \
				    intype c) \
	{ \
		acc = name ## _ ## ts (acc, vget_high_ ## ts (a), \
					    vget_high_ ## ts (b)); \
		return name ## _ ## ts (acc, vget_high_ ## ts (a), \
					     vget_high_ ## ts (c)); \
	}

TEST_MLXL_VEC (vmlal, int16x8_t, int8x16_t, s8)
TEST_MLXL_VEC (vmlal, uint16x8_t, uint8x16_t, u8)
TEST_MLXL_VEC (vmlal, int32x4_t, int16x8_t, s16)
TEST_MLXL_VEC (vmlal, uint32x4_t, uint16x8_t, u16)

TEST_MLXL_VEC (vmlsl, int16x8_t, int8x16_t, s8)
TEST_MLXL_VEC (vmlsl, uint16x8_t, uint8x16_t, u8)
TEST_MLXL_VEC (vmlsl, int32x4_t, int16x8_t, s16)
TEST_MLXL_VEC (vmlsl, uint32x4_t, uint16x8_t, u16)

#define TEST_MLXL_N(name, rettype, intype, ts) \
  rettype test_ ## name ## _ ## ts (rettype acc, intype a, intype b) \
	{ \
		acc = name ## _ ## ts (acc, vget_high_ ## ts (a), b[1]); \
		return name ## _ ## ts (acc, vget_high_ ## ts (a), b[1]); \
	}

TEST_MLXL_N (vmlal_n, int32x4_t, int16x8_t, s16)
TEST_MLXL_N (vmlal_n, uint32x4_t, uint16x8_t, u16)
TEST_MLXL_N (vmlal_n, int64x2_t, int32x4_t, s32)
TEST_MLXL_N (vmlal_n, uint64x2_t, uint32x4_t, u32)

TEST_MLXL_N (vmlsl_n, int32x4_t, int16x8_t, s16)
TEST_MLXL_N (vmlsl_n, uint32x4_t, uint16x8_t, u16)
TEST_MLXL_N (vmlsl_n, int64x2_t, int32x4_t, s32)
TEST_MLXL_N (vmlsl_n, uint64x2_t, uint32x4_t, u32)

TEST_MLXL_N (vqdmlal_n, int32x4_t, int16x8_t, s16)
TEST_MLXL_N (vqdmlal_n, int64x2_t, int32x4_t, s32)

TEST_MLXL_N (vqdmlsl_n, int32x4_t, int16x8_t, s16)
TEST_MLXL_N (vqdmlsl_n, int64x2_t, int32x4_t, s32)

/* { dg-final { scan-assembler-not "dup\\t" } } */
