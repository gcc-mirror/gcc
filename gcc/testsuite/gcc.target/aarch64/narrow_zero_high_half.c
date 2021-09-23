/* { dg-skip-if "" { arm*-*-* } } */
/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

#define TEST_SHIFT(name, rettype, intype, fs, rs) \
  rettype test_ ## name ## _ ## fs ## _zero_high \
		(intype a) \
	{ \
		return vcombine_ ## rs (name ## _ ## fs (a, 4), \
					vdup_n_ ## rs (0)); \
	}

TEST_SHIFT (vshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_SHIFT (vshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_SHIFT (vshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_SHIFT (vshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_SHIFT (vshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_SHIFT (vshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_SHIFT (vrshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_SHIFT (vrshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_SHIFT (vrshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_SHIFT (vrshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_SHIFT (vrshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_SHIFT (vrshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_SHIFT (vqshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_SHIFT (vqshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_SHIFT (vqshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_SHIFT (vqshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_SHIFT (vqshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_SHIFT (vqshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_SHIFT (vqrshrn_n, int8x16_t, int16x8_t, s16, s8)
TEST_SHIFT (vqrshrn_n, int16x8_t, int32x4_t, s32, s16)
TEST_SHIFT (vqrshrn_n, int32x4_t, int64x2_t, s64, s32)
TEST_SHIFT (vqrshrn_n, uint8x16_t, uint16x8_t, u16, u8)
TEST_SHIFT (vqrshrn_n, uint16x8_t, uint32x4_t, u32, u16)
TEST_SHIFT (vqrshrn_n, uint32x4_t, uint64x2_t, u64, u32)

TEST_SHIFT (vqshrun_n, uint8x16_t, int16x8_t, s16, u8)
TEST_SHIFT (vqshrun_n, uint16x8_t, int32x4_t, s32, u16)
TEST_SHIFT (vqshrun_n, uint32x4_t, int64x2_t, s64, u32)

TEST_SHIFT (vqrshrun_n, uint8x16_t, int16x8_t, s16, u8)
TEST_SHIFT (vqrshrun_n, uint16x8_t, int32x4_t, s32, u16)
TEST_SHIFT (vqrshrun_n, uint32x4_t, int64x2_t, s64, u32)

#define TEST_UNARY(name, rettype, intype, fs, rs) \
  rettype test_ ## name ## _ ## fs ## _zero_high \
		(intype a) \
	{ \
		return vcombine_ ## rs (name ## _ ## fs (a), \
					vdup_n_ ## rs (0)); \
	}

TEST_UNARY (vmovn, int8x16_t, int16x8_t, s16, s8)
TEST_UNARY (vmovn, int16x8_t, int32x4_t, s32, s16)
TEST_UNARY (vmovn, int32x4_t, int64x2_t, s64, s32)
TEST_UNARY (vmovn, uint8x16_t, uint16x8_t, u16, u8)
TEST_UNARY (vmovn, uint16x8_t, uint32x4_t, u32, u16)
TEST_UNARY (vmovn, uint32x4_t, uint64x2_t, u64, u32)

TEST_UNARY (vqmovun, uint8x16_t, int16x8_t, s16, u8)
TEST_UNARY (vqmovun, uint16x8_t, int32x4_t, s32, u16)
TEST_UNARY (vqmovun, uint32x4_t, int64x2_t, s64, u32)

TEST_UNARY (vqmovn, int8x16_t, int16x8_t, s16, s8)
TEST_UNARY (vqmovn, int16x8_t, int32x4_t, s32, s16)
TEST_UNARY (vqmovn, int32x4_t, int64x2_t, s64, s32)
TEST_UNARY (vqmovn, uint8x16_t, uint16x8_t, u16, u8)
TEST_UNARY (vqmovn, uint16x8_t, uint32x4_t, u32, u16)
TEST_UNARY (vqmovn, uint32x4_t, uint64x2_t, u64, u32)

#define TEST_ARITH(name, rettype, intype, fs, rs) \
  rettype test_ ## name ## _ ## fs ## _zero_high \
		(intype a, intype b) \
	{ \
		return vcombine_ ## rs (name ## _ ## fs (a, b), \
					vdup_n_ ## rs (0)); \
	}

TEST_ARITH (vaddhn, int8x16_t, int16x8_t, s16, s8)
TEST_ARITH (vaddhn, int16x8_t, int32x4_t, s32, s16)
TEST_ARITH (vaddhn, int32x4_t, int64x2_t, s64, s32)
TEST_ARITH (vaddhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_ARITH (vaddhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_ARITH (vaddhn, uint32x4_t, uint64x2_t, u64, u32)

TEST_ARITH (vraddhn, int8x16_t, int16x8_t, s16, s8)
TEST_ARITH (vraddhn, int16x8_t, int32x4_t, s32, s16)
TEST_ARITH (vraddhn, int32x4_t, int64x2_t, s64, s32)
TEST_ARITH (vraddhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_ARITH (vraddhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_ARITH (vraddhn, uint32x4_t, uint64x2_t, u64, u32)

TEST_ARITH (vsubhn, int8x16_t, int16x8_t, s16, s8)
TEST_ARITH (vsubhn, int16x8_t, int32x4_t, s32, s16)
TEST_ARITH (vsubhn, int32x4_t, int64x2_t, s64, s32)
TEST_ARITH (vsubhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_ARITH (vsubhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_ARITH (vsubhn, uint32x4_t, uint64x2_t, u64, u32)

TEST_ARITH (vrsubhn, int8x16_t, int16x8_t, s16, s8)
TEST_ARITH (vrsubhn, int16x8_t, int32x4_t, s32, s16)
TEST_ARITH (vrsubhn, int32x4_t, int64x2_t, s64, s32)
TEST_ARITH (vrsubhn, uint8x16_t, uint16x8_t, u16, u8)
TEST_ARITH (vrsubhn, uint16x8_t, uint32x4_t, u32, u16)
TEST_ARITH (vrsubhn, uint32x4_t, uint64x2_t, u64, u32)

/* { dg-final { scan-assembler-not "dup\\t" } } */

/* { dg-final { scan-assembler-times "\\tshrn\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\trshrn\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqshrn\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tuqshrn\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tsqrshrn\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tuqrshrn\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tsqshrun\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tsqrshrun\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\txtn\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsqxtun\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tuqxtn\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\tsqxtn\\tv" 3} }  */
/* { dg-final { scan-assembler-times "\\taddhn\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\tsubhn\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\trsubhn\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\traddhn\\tv" 6} }  */
