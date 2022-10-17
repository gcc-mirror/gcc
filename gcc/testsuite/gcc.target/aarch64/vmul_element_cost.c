/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_MUL_UNIFORM(name, q, vectype, ts) \
  vectype test_ ## name ## q ## _ ## ts (vectype a, vectype b, vectype c) \
	{ \
		vectype t0 = name ## q ## _n_ ## ts (a, c[1]); \
		vectype t1 = name ## q ## _n_ ## ts (b, c[1]); \
		return vmul ## q ## _ ## ts (t0, t1); \
	}

TEST_MUL_UNIFORM (vmul, , int16x4_t, s16)
TEST_MUL_UNIFORM (vmul, , uint16x4_t, u16)
TEST_MUL_UNIFORM (vmul, , int32x2_t, s32)
TEST_MUL_UNIFORM (vmul, , uint32x2_t, u32)
TEST_MUL_UNIFORM (vmul, , float32x2_t, f32)
TEST_MUL_UNIFORM (vmul, q, int16x8_t, s16)
TEST_MUL_UNIFORM (vmul, q, uint16x8_t, u16)
TEST_MUL_UNIFORM (vmul, q, int32x4_t, s32)
TEST_MUL_UNIFORM (vmul, q, uint32x4_t, u32)
TEST_MUL_UNIFORM (vmul, q, float32x4_t, f32)
TEST_MUL_UNIFORM (vmul, q, float64x2_t, f64)

#define TEST_MLX_UNIFORM(name, q, vectype, ts) \
  vectype test_ ## name ## q ## _ ## ts (vectype acc, vectype a, vectype b) \
	{ \
		acc = name ## q ## _n_ ## ts (acc, a, b[1]); \
		return name ## q ## _n_ ## ts (acc, a, b[1]); \
	}

TEST_MLX_UNIFORM (vmla, , int16x4_t, s16)
TEST_MLX_UNIFORM (vmla, , uint16x4_t, u16)
TEST_MLX_UNIFORM (vmla, , int32x2_t, s32)
TEST_MLX_UNIFORM (vmla, , uint32x2_t, u32)
TEST_MLX_UNIFORM (vmla, , float32x2_t, f32)
TEST_MLX_UNIFORM (vmla, q, int16x8_t, s16)
TEST_MLX_UNIFORM (vmla, q, uint16x8_t, u16)
TEST_MLX_UNIFORM (vmla, q, int32x4_t, s32)
TEST_MLX_UNIFORM (vmla, q, uint32x4_t, u32)
TEST_MLX_UNIFORM (vmla, q, float32x4_t, f32)

TEST_MLX_UNIFORM (vmls, , int16x4_t, s16)
TEST_MLX_UNIFORM (vmls, , uint16x4_t, u16)
TEST_MLX_UNIFORM (vmls, , int32x2_t, s32)
TEST_MLX_UNIFORM (vmls, , uint32x2_t, u32)
TEST_MLX_UNIFORM (vmls, , float32x2_t, f32)
TEST_MLX_UNIFORM (vmls, q, int16x8_t, s16)
TEST_MLX_UNIFORM (vmls, q, uint16x8_t, u16)
TEST_MLX_UNIFORM (vmls, q, int32x4_t, s32)
TEST_MLX_UNIFORM (vmls, q, uint32x4_t, u32)
TEST_MLX_UNIFORM (vmls, q, float32x4_t, f32)

#define TEST_MUL_LONG(name, rettype, intype, ts, rs) \
  rettype test_ ## name ## ts (intype a, intype b, intype c) \
	{ \
		rettype t0 = name ## ts (a, c[1]); \
		rettype t1 = name ## ts (b, c[1]); \
		return vqaddq ## _ ## rs (t0, t1); \
	}

TEST_MUL_LONG (vmull_n_, int32x4_t, int16x4_t, s16, s32)
TEST_MUL_LONG (vmull_n_, uint32x4_t, uint16x4_t, u16, u32)
TEST_MUL_LONG (vmull_n_, int64x2_t, int32x2_t, s32, s64)
TEST_MUL_LONG (vmull_n_, uint64x2_t, uint32x2_t, u32, u64)

TEST_MUL_LONG (vqdmull_n_, int32x4_t, int16x4_t, s16, s32)
TEST_MUL_LONG (vqdmull_n_, int64x2_t, int32x2_t, s32, s64)

#define TEST_MLX_LONG(name, rettype, intype, ts, rs) \
  rettype test_ ## name ## _ ## ts (rettype acc, intype a, intype b) \
	{ \
		acc = name ## ts (acc, a, b[1]); \
		return name ## ts (acc, a, b[1]); \
	}

TEST_MLX_LONG (vmlal_n_, int32x4_t, int16x4_t, s16, s32)
TEST_MLX_LONG (vmlal_n_, uint32x4_t, uint16x4_t, u16, u32)
TEST_MLX_LONG (vmlal_n_, int64x2_t, int32x2_t, s32, s64)
TEST_MLX_LONG (vmlal_n_, uint64x2_t, uint32x2_t, u32, u64)

TEST_MLX_LONG (vmlsl_n_, int32x4_t, int16x4_t, s16, s32)
TEST_MLX_LONG (vmlsl_n_, uint32x4_t, uint16x4_t, u16, u32)
TEST_MLX_LONG (vmlsl_n_, int64x2_t, int32x2_t, s32, s64)
TEST_MLX_LONG (vmlsl_n_, uint64x2_t, uint32x2_t, u32, u64)

TEST_MLX_LONG (vqdmlal_n_, int32x4_t, int16x4_t, s16, s32)
TEST_MLX_LONG (vqdmlal_n_, int64x2_t, int32x2_t, s32, s64)

TEST_MLX_LONG (vqdmlsl_n_, int32x4_t, int16x4_t, s16, s32)
TEST_MLX_LONG (vqdmlsl_n_, int64x2_t, int32x2_t, s32, s64)

/* { dg-final { scan-assembler-not "dup\\t" } } */
