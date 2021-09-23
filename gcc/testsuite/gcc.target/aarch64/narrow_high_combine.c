/* { dg-skip-if "" { arm*-*-* } } */
/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_1_ARITH(name, rettype, rmwtype, intype, fs, rs) \
  rettype test_1_ ## name ## _ ## fs ## _high_combine \
		(rmwtype a, intype b, intype c) \
	{ \
		return vcombine_ ## rs (a, name ## _ ## fs (b, c)); \
	}

TEST_1_ARITH (vaddhn, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_ARITH (vaddhn, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_ARITH (vaddhn, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_ARITH (vaddhn, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_ARITH (vaddhn, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_ARITH (vaddhn, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_ARITH (vraddhn, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_ARITH (vraddhn, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_ARITH (vraddhn, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_ARITH (vraddhn, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_ARITH (vraddhn, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_ARITH (vraddhn, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_ARITH (vsubhn, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_ARITH (vsubhn, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_ARITH (vsubhn, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_ARITH (vsubhn, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_ARITH (vsubhn, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_ARITH (vsubhn, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_ARITH (vrsubhn, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_ARITH (vrsubhn, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_ARITH (vrsubhn, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_ARITH (vrsubhn, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_ARITH (vrsubhn, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_ARITH (vrsubhn, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

#define TEST_2_ARITH(name, rettype, intype, fs, rs) \
  rettype test_2_ ## name ## _ ## fs ## _high_combine \
		(intype a, intype b, intype c) \
	{ \
		return vcombine_ ## rs (name ## _ ## fs (a, c), \
					name ## _ ## fs (b, c)); \
	}

TEST_2_ARITH (vaddhn, int8x16_t, int16x8_t, s16, s8)
TEST_2_ARITH (vaddhn, int16x8_t, int32x4_t, s32, s16)
TEST_2_ARITH (vaddhn, int32x4_t, int64x2_t, s64, s32)
TEST_2_ARITH (vaddhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_ARITH (vaddhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_ARITH (vaddhn, uint32x4_t,  uint64x2_t, u64, u32)

TEST_2_ARITH (vraddhn, int8x16_t, int16x8_t, s16, s8)
TEST_2_ARITH (vraddhn, int16x8_t, int32x4_t, s32, s16)
TEST_2_ARITH (vraddhn, int32x4_t, int64x2_t, s64, s32)
TEST_2_ARITH (vraddhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_ARITH (vraddhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_ARITH (vraddhn, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_ARITH (vsubhn, int8x16_t, int16x8_t, s16, s8)
TEST_2_ARITH (vsubhn, int16x8_t, int32x4_t, s32, s16)
TEST_2_ARITH (vsubhn, int32x4_t, int64x2_t, s64, s32)
TEST_2_ARITH (vsubhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_ARITH (vsubhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_ARITH (vsubhn, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_ARITH (vrsubhn, int8x16_t, int16x8_t, s16, s8)
TEST_2_ARITH (vrsubhn, int16x8_t, int32x4_t, s32, s16)
TEST_2_ARITH (vrsubhn, int32x4_t, int64x2_t, s64, s32)
TEST_2_ARITH (vrsubhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_ARITH (vrsubhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_ARITH (vrsubhn, uint32x4_t, uint64x2_t, u64, u32)

#define TEST_1_SHIFT(name, rettype, rmwtype, intype, fs, rs) \
  rettype test_1_ ## name ## _ ## fs ## _high_combine \
		(rmwtype a, intype b) \
	{ \
		return vcombine_ ## rs (a, name ## _ ## fs (b, 4)); \
	}

TEST_1_SHIFT (vshrn_n, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_SHIFT (vshrn_n, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_SHIFT (vshrn_n, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_SHIFT (vshrn_n, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_SHIFT (vshrn_n, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_SHIFT (vshrn_n, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_SHIFT (vrshrn_n, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_SHIFT (vrshrn_n, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_SHIFT (vrshrn_n, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_SHIFT (vrshrn_n, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_SHIFT (vrshrn_n, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_SHIFT (vrshrn_n, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_SHIFT (vqshrn_n, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_SHIFT (vqshrn_n, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_SHIFT (vqshrn_n, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_SHIFT (vqshrn_n, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_SHIFT (vqshrn_n, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_SHIFT (vqshrn_n, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_SHIFT (vqrshrn_n, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_SHIFT (vqrshrn_n, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_SHIFT (vqrshrn_n, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_SHIFT (vqrshrn_n, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_SHIFT (vqrshrn_n, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_SHIFT (vqrshrn_n, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_SHIFT (vqshrun_n, uint8x16_t, uint8x8_t, int16x8_t, s16, u8)
TEST_1_SHIFT (vqshrun_n, uint16x8_t, uint16x4_t, int32x4_t, s32, u16)
TEST_1_SHIFT (vqshrun_n, uint32x4_t, uint32x2_t, int64x2_t, s64, u32)

TEST_1_SHIFT (vqrshrun_n, uint8x16_t, uint8x8_t, int16x8_t, s16, u8)
TEST_1_SHIFT (vqrshrun_n, uint16x8_t, uint16x4_t, int32x4_t, s32, u16)
TEST_1_SHIFT (vqrshrun_n, uint32x4_t, uint32x2_t, int64x2_t, s64, u32)

#define TEST_2_SHIFT(name, rettype, intype, fs, rs) \
  rettype test_2_ ## name ## _ ## fs ## _high_combine \
		(intype a, intype b) \
	{ \
		return vcombine_ ## rs (name ## _ ## fs (a, 4), \
					name ## _ ## fs (b, 4)); \
	}

TEST_2_SHIFT (vshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_2_SHIFT (vshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_2_SHIFT (vshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_2_SHIFT (vshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_SHIFT (vshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_SHIFT (vshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_SHIFT (vrshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_2_SHIFT (vrshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_2_SHIFT (vrshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_2_SHIFT (vrshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_SHIFT (vrshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_SHIFT (vrshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_SHIFT (vqshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_2_SHIFT (vqshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_2_SHIFT (vqshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_2_SHIFT (vqshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_SHIFT (vqshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_SHIFT (vqshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_SHIFT (vqrshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_2_SHIFT (vqrshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_2_SHIFT (vqrshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_2_SHIFT (vqrshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_SHIFT (vqrshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_SHIFT (vqrshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_SHIFT (vqshrun_n, uint8x16_t, int16x8_t, s16, u8)
TEST_2_SHIFT (vqshrun_n, uint16x8_t, int32x4_t, s32, u16)
TEST_2_SHIFT (vqshrun_n, uint32x4_t, int64x2_t, s64, u32)

TEST_2_SHIFT (vqrshrun_n, uint8x16_t, int16x8_t, s16, u8)
TEST_2_SHIFT (vqrshrun_n, uint16x8_t, int32x4_t, s32, u16)
TEST_2_SHIFT (vqrshrun_n, uint32x4_t, int64x2_t, s64, u32)

#define TEST_1_UNARY(name, rettype, rmwtype, intype, fs, rs) \
  rettype test_1_ ## name ## _ ## fs ## _high_combine \
		(rmwtype a, intype b) \
	{ \
		return vcombine_ ## rs (a, name ## _ ## fs (b)); \
	}

TEST_1_UNARY (vmovn, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_UNARY (vmovn, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_UNARY (vmovn, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_UNARY (vmovn, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_UNARY (vmovn, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_UNARY (vmovn, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_UNARY (vqmovn, int8x16_t, int8x8_t, int16x8_t, s16, s8)
TEST_1_UNARY (vqmovn, int16x8_t, int16x4_t, int32x4_t, s32, s16)
TEST_1_UNARY (vqmovn, int32x4_t, int32x2_t, int64x2_t, s64, s32)
TEST_1_UNARY (vqmovn, uint8x16_t, uint8x8_t, uint16x8_t, u16, u8)
TEST_1_UNARY (vqmovn, uint16x8_t, uint16x4_t, uint32x4_t, u32, u16)
TEST_1_UNARY (vqmovn, uint32x4_t, uint32x2_t, uint64x2_t, u64, u32)

TEST_1_UNARY (vqmovun, uint8x16_t, uint8x8_t, int16x8_t, s16, u8)
TEST_1_UNARY (vqmovun, uint16x8_t, uint16x4_t, int32x4_t, s32, u16)
TEST_1_UNARY (vqmovun, uint32x4_t, uint32x2_t, int64x2_t, s64, u32)

#define TEST_2_UNARY(name, rettype, intype, fs, rs) \
  rettype test_2_ ## name ## _ ## fs ## _high_combine \
		(intype a, intype b) \
	{ \
		return vcombine_ ## rs (name ## _ ## fs (a), \
					name ## _ ## fs (b)); \
	}

TEST_2_UNARY (vmovn, int8x16_t, int16x8_t, s16, s8)
TEST_2_UNARY (vmovn, int16x8_t, int32x4_t, s32, s16)
TEST_2_UNARY (vmovn, int32x4_t, int64x2_t, s64, s32)
TEST_2_UNARY (vmovn, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_UNARY (vmovn, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_UNARY (vmovn, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_UNARY (vqmovn, int8x16_t, int16x8_t, s16, s8)
TEST_2_UNARY (vqmovn, int16x8_t, int32x4_t, s32, s16)
TEST_2_UNARY (vqmovn, int32x4_t, int64x2_t, s64, s32)
TEST_2_UNARY (vqmovn, uint8x16_t, uint16x8_t, u16, u8)
TEST_2_UNARY (vqmovn, uint16x8_t, uint32x4_t, u32, u16)
TEST_2_UNARY (vqmovn, uint32x4_t, uint64x2_t, u64, u32)

TEST_2_UNARY (vqmovun, uint8x16_t, int16x8_t, s16, u8)
TEST_2_UNARY (vqmovun, uint16x8_t, int32x4_t, s32, u16)
TEST_2_UNARY (vqmovun, uint32x4_t, int64x2_t, s64, u32)

/* { dg-final { scan-assembler-times "\\taddhn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\tsubhn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\trsubhn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\traddhn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\trshrn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\tshrn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\tsqshrun2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqrshrun2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqshrn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tuqshrn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqrshrn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tuqrshrn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\txtn2\\tv" 12} }  */
/* { dg-final { scan-assembler-times "\\tuqxtn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqxtn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqxtun2\\tv" 6} }  */
