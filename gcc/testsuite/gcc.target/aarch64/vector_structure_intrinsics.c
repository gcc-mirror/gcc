/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+bf16" } */

#include <arm_neon.h>

#define TEST_TBL(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (tbltype a, idxtype b) \
	{ \
		return name ## _ ## ts (a, b); \
	}

TEST_TBL (vqtbl2, int8x8_t, int8x16x2_t, uint8x8_t, s8)
TEST_TBL (vqtbl2, uint8x8_t, uint8x16x2_t, uint8x8_t, u8)
TEST_TBL (vqtbl2, poly8x8_t, poly8x16x2_t, uint8x8_t, p8)

TEST_TBL (vqtbl2q, int8x16_t, int8x16x2_t, uint8x16_t, s8)
TEST_TBL (vqtbl2q, uint8x16_t, uint8x16x2_t, uint8x16_t, u8)
TEST_TBL (vqtbl2q, poly8x16_t, poly8x16x2_t, uint8x16_t, p8)

TEST_TBL (vqtbl4, int8x8_t, int8x16x4_t, uint8x8_t, s8)
TEST_TBL (vqtbl4, uint8x8_t, uint8x16x4_t, uint8x8_t, u8)
TEST_TBL (vqtbl4, poly8x8_t, poly8x16x4_t, uint8x8_t, p8)

TEST_TBL (vqtbl4q, int8x16_t, int8x16x4_t, uint8x16_t, s8)
TEST_TBL (vqtbl4q, uint8x16_t, uint8x16x4_t, uint8x16_t, u8)
TEST_TBL (vqtbl4q, poly8x16_t, poly8x16x4_t, uint8x16_t, p8)

#define TEST_TBL3(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (idxtype a, tbltype b) \
	{ \
		return name ## _ ## ts (b, a); \
	}

TEST_TBL3 (vqtbl3, int8x8_t, int8x16x3_t, uint8x8_t, s8)
TEST_TBL3 (vqtbl3, uint8x8_t, uint8x16x3_t, uint8x8_t, u8)
TEST_TBL3 (vqtbl3, poly8x8_t, poly8x16x3_t, uint8x8_t, p8)

TEST_TBL3 (vqtbl3q, int8x16_t, int8x16x3_t, uint8x16_t, s8)
TEST_TBL3 (vqtbl3q, uint8x16_t, uint8x16x3_t, uint8x16_t, u8)
TEST_TBL3 (vqtbl3q, poly8x16_t, poly8x16x3_t, uint8x16_t, p8)

#define TEST_TBX2(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, idxtype b, tbltype c) \
	{ \
		return name ## _ ## ts (a, c, b); \
	}

TEST_TBX2 (vqtbx2, int8x8_t, int8x16x2_t, uint8x8_t, s8)
TEST_TBX2 (vqtbx2, uint8x8_t, uint8x16x2_t, uint8x8_t, u8)
TEST_TBX2 (vqtbx2, poly8x8_t, poly8x16x2_t, uint8x8_t, p8)

TEST_TBX2 (vqtbx2q, int8x16_t, int8x16x2_t, uint8x16_t, s8)
TEST_TBX2 (vqtbx2q, uint8x16_t, uint8x16x2_t, uint8x16_t, u8)
TEST_TBX2 (vqtbx2q, poly8x16_t, poly8x16x2_t, uint8x16_t, p8)

#define TEST_TBX3(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, tbltype b, idxtype c) \
	{ \
		return name ## _ ## ts (a, b, c); \
	}

TEST_TBX3 (vqtbx3, int8x8_t, int8x16x3_t, uint8x8_t, s8)
TEST_TBX3 (vqtbx3, uint8x8_t, uint8x16x3_t, uint8x8_t, u8)
TEST_TBX3 (vqtbx3, poly8x8_t, poly8x16x3_t, uint8x8_t, p8)

TEST_TBX3 (vqtbx3q, int8x16_t, int8x16x3_t, uint8x16_t, s8)
TEST_TBX3 (vqtbx3q, uint8x16_t, uint8x16x3_t, uint8x16_t, u8)
TEST_TBX3 (vqtbx3q, poly8x16_t, poly8x16x3_t, uint8x16_t, p8)

#define TEST_TBX4(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, idxtype b, idxtype dummy1, \
				    idxtype dummy2, tbltype c) \
	{ \
		return name ## _ ## ts (a, c, b); \
	}

TEST_TBX4 (vqtbx4, int8x8_t, int8x16x4_t, uint8x8_t, s8)
TEST_TBX4 (vqtbx4, uint8x8_t, uint8x16x4_t, uint8x8_t, u8)
TEST_TBX4 (vqtbx4, poly8x8_t, poly8x16x4_t, uint8x8_t, p8)

TEST_TBX4 (vqtbx4q, int8x16_t, int8x16x4_t, uint8x16_t, s8)
TEST_TBX4 (vqtbx4q, uint8x16_t, uint8x16x4_t, uint8x16_t, u8)
TEST_TBX4 (vqtbx4q, poly8x16_t, poly8x16x4_t, uint8x16_t, p8)

#define TEST_STX(name, tbltype, ptrtype, ts) \
  void test_ ## name ## _ ## ts (ptrtype a, tbltype b) \
	{ \
		name ## _ ## ts (a, b); \
	}

TEST_STX (vst4q, int8x16x4_t, int8_t*, s8);
TEST_STX (vst4q, uint8x16x4_t, uint8_t*, u8);
TEST_STX (vst4q, poly8x16x4_t, poly8_t*, p8);
TEST_STX (vst4q, int16x8x4_t, int16_t*, s16);
TEST_STX (vst4q, uint16x8x4_t, uint16_t*, u16);
TEST_STX (vst4q, poly16x8x4_t, poly16_t*, p16);
TEST_STX (vst4q, float16x8x4_t, float16_t*, f16);
TEST_STX (vst4q, bfloat16x8x4_t, bfloat16_t*, bf16);
TEST_STX (vst4q, int32x4x4_t, int32_t*, s32);
TEST_STX (vst4q, uint32x4x4_t, uint32_t*, u32);
TEST_STX (vst4q, float32x4x4_t, float32_t*, f32);
TEST_STX (vst4q, int64x2x4_t, int64_t*, s64);
TEST_STX (vst4q, uint64x2x4_t, uint64_t*, u64);
TEST_STX (vst4q, float64x2x4_t, float64_t*, f64);
TEST_STX (vst4q, poly64x2x4_t, poly64_t*, p64);

TEST_STX (vst2q, int8x16x2_t, int8_t*, s8);
TEST_STX (vst2q, uint8x16x2_t, uint8_t*, u8);
TEST_STX (vst2q, poly8x16x2_t, poly8_t*, p8);
TEST_STX (vst2q, int16x8x2_t, int16_t*, s16);
TEST_STX (vst2q, uint16x8x2_t, uint16_t*, u16);
TEST_STX (vst2q, poly16x8x2_t, poly16_t*, p16);
TEST_STX (vst2q, float16x8x2_t, float16_t*, f16);
TEST_STX (vst2q, bfloat16x8x2_t, bfloat16_t*, bf16);
TEST_STX (vst2q, int32x4x2_t, int32_t*, s32);
TEST_STX (vst2q, uint32x4x2_t, uint32_t*, u32);
TEST_STX (vst2q, float32x4x2_t, float32_t*, f32);
TEST_STX (vst2q, int64x2x2_t, int64_t*, s64);
TEST_STX (vst2q, uint64x2x2_t, uint64_t*, u64);
TEST_STX (vst2q, float64x2x2_t, float64_t*, f64);
TEST_STX (vst2q, poly64x2x2_t, poly64_t*, p64);

#define TEST_ST3(name, tbltype, ptrtype, ts) \
  void test_ ## name ## _ ## ts (ptrtype a, int8x8_t dummy, tbltype b) \
	{ \
		name ## _ ## ts (a, b); \
	}

TEST_ST3 (vst3q, int8x16x3_t, int8_t*, s8);
TEST_ST3 (vst3q, uint8x16x3_t, uint8_t*, u8);
TEST_ST3 (vst3q, poly8x16x3_t, poly8_t*, p8);
TEST_ST3 (vst3q, int16x8x3_t, int16_t*, s16);
TEST_ST3 (vst3q, uint16x8x3_t, uint16_t*, u16);
TEST_ST3 (vst3q, poly16x8x3_t, poly16_t*, p16);
TEST_ST3 (vst3q, float16x8x3_t, float16_t*, f16);
TEST_ST3 (vst3q, bfloat16x8x3_t, bfloat16_t*, bf16);
TEST_ST3 (vst3q, int32x4x3_t, int32_t*, s32);
TEST_ST3 (vst3q, uint32x4x3_t, uint32_t*, u32);
TEST_ST3 (vst3q, float32x4x3_t, float32_t*, f32);
TEST_ST3 (vst3q, int64x2x3_t, int64_t*, s64);
TEST_ST3 (vst3q, uint64x2x3_t, uint64_t*, u64);
TEST_ST3 (vst3q, float64x2x3_t, float64_t*, f64);
TEST_ST3 (vst3q, poly64x2x3_t, poly64_t*, p64);

#define TEST_STX_LANE(name, tbltype, ptrtype, ts) \
  void test_ ## name ## _ ## ts (ptrtype a, tbltype b) \
	{ \
		name ## _ ## ts (a, b, 1); \
	}

TEST_STX_LANE (vst4q_lane, int8x16x4_t, int8_t*, s8);
TEST_STX_LANE (vst4q_lane, uint8x16x4_t, uint8_t*, u8);
TEST_STX_LANE (vst4q_lane, poly8x16x4_t, poly8_t*, p8);
TEST_STX_LANE (vst4q_lane, int16x8x4_t, int16_t*, s16);
TEST_STX_LANE (vst4q_lane, uint16x8x4_t, uint16_t*, u16);
TEST_STX_LANE (vst4q_lane, poly16x8x4_t, poly16_t*, p16);
TEST_STX_LANE (vst4q_lane, float16x8x4_t, float16_t*, f16);
TEST_STX_LANE (vst4q_lane, bfloat16x8x4_t, bfloat16_t*, bf16);
TEST_STX_LANE (vst4q_lane, int32x4x4_t, int32_t*, s32);
TEST_STX_LANE (vst4q_lane, uint32x4x4_t, uint32_t*, u32);
TEST_STX_LANE (vst4q_lane, float32x4x4_t, float32_t*, f32);
TEST_STX_LANE (vst4q_lane, int64x2x4_t, int64_t*, s64);
TEST_STX_LANE (vst4q_lane, uint64x2x4_t, uint64_t*, u64);
TEST_STX_LANE (vst4q_lane, float64x2x4_t, float64_t*, f64);
TEST_STX_LANE (vst4q_lane, poly64x2x4_t, poly64_t*, p64);

TEST_STX_LANE (vst2q_lane, int8x16x2_t, int8_t*, s8);
TEST_STX_LANE (vst2q_lane, uint8x16x2_t, uint8_t*, u8);
TEST_STX_LANE (vst2q_lane, poly8x16x2_t, poly8_t*, p8);
TEST_STX_LANE (vst2q_lane, int16x8x2_t, int16_t*, s16);
TEST_STX_LANE (vst2q_lane, uint16x8x2_t, uint16_t*, u16);
TEST_STX_LANE (vst2q_lane, poly16x8x2_t, poly16_t*, p16);
TEST_STX_LANE (vst2q_lane, float16x8x2_t, float16_t*, f16);
TEST_STX_LANE (vst2q_lane, bfloat16x8x2_t, bfloat16_t*, bf16);
TEST_STX_LANE (vst2q_lane, int32x4x2_t, int32_t*, s32);
TEST_STX_LANE (vst2q_lane, uint32x4x2_t, uint32_t*, u32);
TEST_STX_LANE (vst2q_lane, float32x4x2_t, float32_t*, f32);
TEST_STX_LANE (vst2q_lane, int64x2x2_t, int64_t*, s64);
TEST_STX_LANE (vst2q_lane, uint64x2x2_t, uint64_t*, u64);
TEST_STX_LANE (vst2q_lane, float64x2x2_t, float64_t*, f64);
TEST_STX_LANE (vst2q_lane, poly64x2x2_t, poly64_t*, p64);

#define TEST_ST3_LANE(name, tbltype, ptrtype, ts) \
  void test_ ## name ## _ ## ts (ptrtype a, int8x8_t dummy, tbltype b) \
	{ \
		name ## _ ## ts (a, b, 1); \
	}

TEST_ST3_LANE (vst3q_lane, int8x16x3_t, int8_t*, s8);
TEST_ST3_LANE (vst3q_lane, uint8x16x3_t, uint8_t*, u8);
TEST_ST3_LANE (vst3q_lane, poly8x16x3_t, poly8_t*, p8);
TEST_ST3_LANE (vst3q_lane, int16x8x3_t, int16_t*, s16);
TEST_ST3_LANE (vst3q_lane, uint16x8x3_t, uint16_t*, u16);
TEST_ST3_LANE (vst3q_lane, poly16x8x3_t, poly16_t*, p16);
TEST_ST3_LANE (vst3q_lane, float16x8x3_t, float16_t*, f16);
TEST_ST3_LANE (vst3q_lane, bfloat16x8x3_t, bfloat16_t*, bf16);
TEST_ST3_LANE (vst3q_lane, int32x4x3_t, int32_t*, s32);
TEST_ST3_LANE (vst3q_lane, uint32x4x3_t, uint32_t*, u32);
TEST_ST3_LANE (vst3q_lane, float32x4x3_t, float32_t*, f32);
TEST_ST3_LANE (vst3q_lane, int64x2x3_t, int64_t*, s64);
TEST_ST3_LANE (vst3q_lane, uint64x2x3_t, uint64_t*, u64);
TEST_ST3_LANE (vst3q_lane, float64x2x3_t, float64_t*, f64);
TEST_ST3_LANE (vst3q_lane, poly64x2x3_t, poly64_t*, p64);

#define TEST_ST1xN(name, tbltype, ptrtype, ts, xn) \
  void test_ ## name ## _ ## ts ## _ ## xn (ptrtype a, tbltype b) \
	{ \
		name ## _ ## ts ## _ ## xn (a, b); \
	}

TEST_ST1xN (vst1q, int8x16x4_t, int8_t*, s8, x4);
TEST_ST1xN (vst1q, uint8x16x4_t, uint8_t*, u8, x4);
TEST_ST1xN (vst1q, poly8x16x4_t, poly8_t*, p8, x4);
TEST_ST1xN (vst1q, int16x8x4_t, int16_t*, s16, x4);
TEST_ST1xN (vst1q, uint16x8x4_t, uint16_t*, u16, x4);
TEST_ST1xN (vst1q, poly16x8x4_t, poly16_t*, p16, x4);
TEST_ST1xN (vst1q, float16x8x4_t, float16_t*, f16, x4);
TEST_ST1xN (vst1q, bfloat16x8x4_t, bfloat16_t*, bf16, x4);
TEST_ST1xN (vst1q, int32x4x4_t, int32_t*, s32, x4);
TEST_ST1xN (vst1q, uint32x4x4_t, uint32_t*, u32, x4);
TEST_ST1xN (vst1q, float32x4x4_t, float32_t*, f32, x4);
TEST_ST1xN (vst1q, int64x2x4_t, int64_t*, s64, x4);
TEST_ST1xN (vst1q, uint64x2x4_t, uint64_t*, u64, x4);
TEST_ST1xN (vst1q, poly64x2x4_t, poly64_t*, p64, x4);
TEST_ST1xN (vst1q, float64x2x4_t, float64_t*, f64, x4);

TEST_ST1xN (vst1q, int8x16x2_t, int8_t*, s8, x2);
TEST_ST1xN (vst1q, uint8x16x2_t, uint8_t*, u8, x2);
TEST_ST1xN (vst1q, poly8x16x2_t, poly8_t*, p8, x2);
TEST_ST1xN (vst1q, int16x8x2_t, int16_t*, s16, x2);
TEST_ST1xN (vst1q, uint16x8x2_t, uint16_t*, u16, x2);
TEST_ST1xN (vst1q, poly16x8x2_t, poly16_t*, p16, x2);
TEST_ST1xN (vst1q, float16x8x2_t, float16_t*, f16, x2);
TEST_ST1xN (vst1q, bfloat16x8x2_t, bfloat16_t*, bf16, x2);
TEST_ST1xN (vst1q, int32x4x2_t, int32_t*, s32, x2);
TEST_ST1xN (vst1q, uint32x4x2_t, uint32_t*, u32, x2);
TEST_ST1xN (vst1q, float32x4x2_t, float32_t*, f32, x2);
TEST_ST1xN (vst1q, int64x2x2_t, int64_t*, s64, x2);
TEST_ST1xN (vst1q, uint64x2x2_t, uint64_t*, u64, x2);
TEST_ST1xN (vst1q, poly64x2x2_t, poly64_t*, p64, x2);
TEST_ST1xN (vst1q, float64x2x2_t, float64_t*, f64, x2);

#define TEST_ST1x3(name, tbltype, ptrtype, ts, xn) \
  void test_ ## name ## _ ## ts ## _ ## xn (ptrtype a, int8x8_t dummy, \
					    tbltype b) \
	{ \
		name ## _ ## ts ## _ ## xn (a, b); \
	}

TEST_ST1x3 (vst1q, int8x16x3_t, int8_t*, s8, x3);
TEST_ST1x3 (vst1q, uint8x16x3_t, uint8_t*, u8, x3);
TEST_ST1x3 (vst1q, poly8x16x3_t, poly8_t*, p8, x3);
TEST_ST1x3 (vst1q, int16x8x3_t, int16_t*, s16, x3);
TEST_ST1x3 (vst1q, uint16x8x3_t, uint16_t*, u16, x3);
TEST_ST1x3 (vst1q, poly16x8x3_t, poly16_t*, p16, x3);
TEST_ST1x3 (vst1q, float16x8x3_t, float16_t*, f16, x3);
TEST_ST1x3 (vst1q, bfloat16x8x3_t, bfloat16_t*, bf16, x3);
TEST_ST1x3 (vst1q, int32x4x3_t, int32_t*, s32, x3);
TEST_ST1x3 (vst1q, uint32x4x3_t, uint32_t*, u32, x3);
TEST_ST1x3 (vst1q, float32x4x3_t, float32_t*, f32, x3);
TEST_ST1x3 (vst1q, int64x2x3_t, int64_t*, s64, x3);
TEST_ST1x3 (vst1q, uint64x2x3_t, uint64_t*, u64, x3);
TEST_ST1x3 (vst1q, poly64x2x3_t, poly64_t*, p64, x3);
TEST_ST1x3 (vst1q, float64x2x3_t, float64_t*, f64, x3);

/* { dg-final { scan-assembler-not "mov\\t" { target aarch64_little_endian } } } */

/* { dg-final { scan-assembler-times "tbl\\t" 18} }  */
/* { dg-final { scan-assembler-times "tbx\\t" 18} }  */
/* { dg-final { scan-assembler-times "st4\\t" 30} }  */
/* { dg-final { scan-assembler-times "st3\\t" 30} }  */
/* { dg-final { scan-assembler-times "st2\\t" 30} }  */
/* { dg-final { scan-assembler-times "st1\\t" 45} }  */
