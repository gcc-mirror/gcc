/* { dg-do compile } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

#define TWO(name, rettype, rmwtype, intype, fs) \
  rettype test_ ## name ## _ ## fs \
		(rmwtype a, intype b, intype c) \
	{ \
		return name ## _ ## fs (a, b, c); \
	}

TWO (vsubhn_high, int8x16_t, int8x8_t, int16x8_t,  s16)
TWO (vsubhn_high, int16x8_t, int16x4_t, int32x4_t, s32)
TWO (vsubhn_high, int32x4_t, int32x2_t, int64x2_t, s64)
TWO (vsubhn_high, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWO (vsubhn_high, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWO (vsubhn_high, uint32x4_t, uint32x2_t, uint64x2_t, u64)

TWO (vaddhn_high, int8x16_t, int8x8_t, int16x8_t,  s16)
TWO (vaddhn_high, int16x8_t, int16x4_t, int32x4_t, s32)
TWO (vaddhn_high, int32x4_t, int32x2_t, int64x2_t, s64)
TWO (vaddhn_high, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWO (vaddhn_high, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWO (vaddhn_high, uint32x4_t, uint32x2_t, uint64x2_t, u64)

TWO (vrsubhn_high, int8x16_t, int8x8_t, int16x8_t,  s16)
TWO (vrsubhn_high, int16x8_t, int16x4_t, int32x4_t, s32)
TWO (vrsubhn_high, int32x4_t, int32x2_t, int64x2_t, s64)
TWO (vrsubhn_high, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWO (vrsubhn_high, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWO (vrsubhn_high, uint32x4_t, uint32x2_t, uint64x2_t, u64)

TWO (vraddhn_high, int8x16_t, int8x8_t, int16x8_t,  s16)
TWO (vraddhn_high, int16x8_t, int16x4_t, int32x4_t, s32)
TWO (vraddhn_high, int32x4_t, int32x2_t, int64x2_t, s64)
TWO (vraddhn_high, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWO (vraddhn_high, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWO (vraddhn_high, uint32x4_t, uint32x2_t, uint64x2_t, u64)

#define TWOn(name, rettype, rmwtype, intype, fs) \
  rettype test_ ## name ## _ ## fs \
		(rmwtype a, intype b) \
	{ \
		return name ## _ ## fs (a, b, 4); \
	}

TWOn (vrshrn_high_n, int8x16_t, int8x8_t, int16x8_t,  s16)
TWOn (vrshrn_high_n, int16x8_t, int16x4_t, int32x4_t, s32)
TWOn (vrshrn_high_n, int32x4_t, int32x2_t, int64x2_t, s64)
TWOn (vrshrn_high_n, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWOn (vrshrn_high_n, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWOn (vrshrn_high_n, uint32x4_t, uint32x2_t, uint64x2_t, u64)

TWOn (vshrn_high_n, int8x16_t, int8x8_t, int16x8_t,  s16)
TWOn (vshrn_high_n, int16x8_t, int16x4_t, int32x4_t, s32)
TWOn (vshrn_high_n, int32x4_t, int32x2_t, int64x2_t, s64)
TWOn (vshrn_high_n, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWOn (vshrn_high_n, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWOn (vshrn_high_n, uint32x4_t, uint32x2_t, uint64x2_t, u64)

TWOn (vqshrun_high_n, uint8x16_t, uint8x8_t, int16x8_t,  s16)
TWOn (vqshrun_high_n, uint16x8_t, uint16x4_t, int32x4_t, s32)
TWOn (vqshrun_high_n, uint32x4_t, uint32x2_t, int64x2_t, s64)

TWOn (vqrshrun_high_n, uint8x16_t, uint8x8_t, int16x8_t,  s16)
TWOn (vqrshrun_high_n, uint16x8_t, uint16x4_t, int32x4_t, s32)
TWOn (vqrshrun_high_n, uint32x4_t, uint32x2_t, int64x2_t, s64)

TWOn (vqshrn_high_n, int8x16_t, int8x8_t, int16x8_t,  s16)
TWOn (vqshrn_high_n, int16x8_t, int16x4_t, int32x4_t, s32)
TWOn (vqshrn_high_n, int32x4_t, int32x2_t, int64x2_t, s64)
TWOn (vqshrn_high_n, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWOn (vqshrn_high_n, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWOn (vqshrn_high_n, uint32x4_t, uint32x2_t, uint64x2_t, u64)

TWOn (vqrshrn_high_n, int8x16_t, int8x8_t, int16x8_t,  s16)
TWOn (vqrshrn_high_n, int16x8_t, int16x4_t, int32x4_t, s32)
TWOn (vqrshrn_high_n, int32x4_t, int32x2_t, int64x2_t, s64)
TWOn (vqrshrn_high_n, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
TWOn (vqrshrn_high_n, uint16x8_t, uint16x4_t, uint32x4_t, u32)
TWOn (vqrshrn_high_n, uint32x4_t, uint32x2_t, uint64x2_t, u64)

#define ONE(name, rettype, rmwtype, intype, fs) \
  rettype test_ ## name ## _ ## fs \
		(rmwtype a, intype b) \
	{ \
		return name ## _ ## fs (a, b); \
	}

ONE (vqmovn_high, int8x16_t, int8x8_t, int16x8_t,  s16)
ONE (vqmovn_high, int16x8_t, int16x4_t, int32x4_t, s32)
ONE (vqmovn_high, int32x4_t, int32x2_t, int64x2_t, s64)
ONE (vqmovn_high, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
ONE (vqmovn_high, uint16x8_t, uint16x4_t, uint32x4_t, u32)
ONE (vqmovn_high, uint32x4_t, uint32x2_t, uint64x2_t, u64)

ONE (vqmovun_high, uint8x16_t, uint8x8_t, int16x8_t,  s16)
ONE (vqmovun_high, uint16x8_t, uint16x4_t, int32x4_t, s32)
ONE (vqmovun_high, uint32x4_t, uint32x2_t, int64x2_t, s64)

ONE (vmovn_high, int8x16_t, int8x8_t, int16x8_t,  s16)
ONE (vmovn_high, int16x8_t, int16x4_t, int32x4_t, s32)
ONE (vmovn_high, int32x4_t, int32x2_t, int64x2_t, s64)
ONE (vmovn_high, uint8x16_t, uint8x8_t, uint16x8_t,  u16)
ONE (vmovn_high, uint16x8_t, uint16x4_t, uint32x4_t, u32)
ONE (vmovn_high, uint32x4_t, uint32x2_t, uint64x2_t, u64)


/* { dg-final { scan-assembler-times "\\tsubhn2 v" 6} }  */
/* { dg-final { scan-assembler-times "\\taddhn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "rsubhn2 v" 6} }  */
/* { dg-final { scan-assembler-times "raddhn2\\tv" 6} }  */
/* { dg-final { scan-assembler-times "\\trshrn2 v" 6} }  */
/* { dg-final { scan-assembler-times "\\tshrn2 v" 6} }  */
/* { dg-final { scan-assembler-times "sqshrun2 v" 3} }  */
/* { dg-final { scan-assembler-times "sqrshrun2 v" 3} }  */
/* { dg-final { scan-assembler-times "sqshrn2 v" 3} }  */
/* { dg-final { scan-assembler-times "uqshrn2 v" 3} }  */
/* { dg-final { scan-assembler-times "sqrshrn2 v" 3} }  */
/* { dg-final { scan-assembler-times "uqrshrn2 v" 3} }  */
/* { dg-final { scan-assembler-times "uqxtn2 v" 3} }  */
/* { dg-final { scan-assembler-times "sqxtn2 v" 3} }  */
/* { dg-final { scan-assembler-times "sqxtun2 v" 3} }  */
/* { dg-final { scan-assembler-times "\\txtn2 v" 6} }  */
