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

TEST_TBL (vqtbl3, int8x8_t, int8x16x3_t, uint8x8_t, s8)
TEST_TBL (vqtbl3, uint8x8_t, uint8x16x3_t, uint8x8_t, u8)
TEST_TBL (vqtbl3, poly8x8_t, poly8x16x3_t, uint8x8_t, p8)

TEST_TBL (vqtbl3q, int8x16_t, int8x16x3_t, uint8x16_t, s8)
TEST_TBL (vqtbl3q, uint8x16_t, uint8x16x3_t, uint8x16_t, u8)
TEST_TBL (vqtbl3q, poly8x16_t, poly8x16x3_t, uint8x16_t, p8)

TEST_TBL (vqtbl4, int8x8_t, int8x16x4_t, uint8x8_t, s8)
TEST_TBL (vqtbl4, uint8x8_t, uint8x16x4_t, uint8x8_t, u8)
TEST_TBL (vqtbl4, poly8x8_t, poly8x16x4_t, uint8x8_t, p8)

TEST_TBL (vqtbl4q, int8x16_t, int8x16x4_t, uint8x16_t, s8)
TEST_TBL (vqtbl4q, uint8x16_t, uint8x16x4_t, uint8x16_t, u8)
TEST_TBL (vqtbl4q, poly8x16_t, poly8x16x4_t, uint8x16_t, p8)

#define TEST_TBX(name, rettype, tbltype, idxtype, ts) \
  rettype test_ ## name ## _ ## ts (rettype a, tbltype b, idxtype c) \
	{ \
		return name ## _ ## ts (a, b, c); \
	}

TEST_TBX (vqtbx2, int8x8_t, int8x16x2_t, uint8x8_t, s8)
TEST_TBX (vqtbx2, uint8x8_t, uint8x16x2_t, uint8x8_t, u8)
TEST_TBX (vqtbx2, poly8x8_t, poly8x16x2_t, uint8x8_t, p8)

TEST_TBX (vqtbx2q, int8x16_t, int8x16x2_t, uint8x16_t, s8)
TEST_TBX (vqtbx2q, uint8x16_t, uint8x16x2_t, uint8x16_t, u8)
TEST_TBX (vqtbx2q, poly8x16_t, poly8x16x2_t, uint8x16_t, p8)

TEST_TBX (vqtbx3, int8x8_t, int8x16x3_t, uint8x8_t, s8)
TEST_TBX (vqtbx3, uint8x8_t, uint8x16x3_t, uint8x8_t, u8)
TEST_TBX (vqtbx3, poly8x8_t, poly8x16x3_t, uint8x8_t, p8)

TEST_TBX (vqtbx3q, int8x16_t, int8x16x3_t, uint8x16_t, s8)
TEST_TBX (vqtbx3q, uint8x16_t, uint8x16x3_t, uint8x16_t, u8)
TEST_TBX (vqtbx3q, poly8x16_t, poly8x16x3_t, uint8x16_t, p8)

TEST_TBX (vqtbx4, int8x8_t, int8x16x4_t, uint8x8_t, s8)
TEST_TBX (vqtbx4, uint8x8_t, uint8x16x4_t, uint8x8_t, u8)
TEST_TBX (vqtbx4, poly8x8_t, poly8x16x4_t, uint8x8_t, p8)

TEST_TBX (vqtbx4q, int8x16_t, int8x16x4_t, uint8x16_t, s8)
TEST_TBX (vqtbx4q, uint8x16_t, uint8x16x4_t, uint8x16_t, u8)
TEST_TBX (vqtbx4q, poly8x16_t, poly8x16x4_t, uint8x16_t, p8)

#define TEST_STX(name, tbltype, ptrtype, ts) \
  void test_ ## name ## _ ## ts (ptrtype a, tbltype b) \
	{ \
		name ## _ ## ts (a, b); \
	}

TEST_STX (vst2, int8x8x2_t, int8_t*, s8);
TEST_STX (vst2, uint8x8x2_t, uint8_t*, u8);
TEST_STX (vst2, poly8x8x2_t, poly8_t*, p8);
TEST_STX (vst2, int16x4x2_t, int16_t*, s16);
TEST_STX (vst2, uint16x4x2_t, uint16_t*, u16);
TEST_STX (vst2, poly16x4x2_t, poly16_t*, p16);
TEST_STX (vst2, float16x4x2_t, float16_t*, f16);
TEST_STX (vst2, bfloat16x4x2_t, bfloat16_t*, bf16);
TEST_STX (vst2, int32x2x2_t, int32_t*, s32);
TEST_STX (vst2, uint32x2x2_t, uint32_t*, u32);
TEST_STX (vst2, float32x2x2_t, float32_t*, f32);
TEST_STX (vst2, int64x1x2_t, int64_t*, s64);
TEST_STX (vst2, uint64x1x2_t, uint64_t*, u64);
TEST_STX (vst2, float64x1x2_t, float64_t*, f64);
TEST_STX (vst2, poly64x1x2_t, poly64_t*, p64);

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

TEST_STX (vst3, int8x8x3_t, int8_t*, s8);
TEST_STX (vst3, uint8x8x3_t, uint8_t*, u8);
TEST_STX (vst3, poly8x8x3_t, poly8_t*, p8);
TEST_STX (vst3, int16x4x3_t, int16_t*, s16);
TEST_STX (vst3, uint16x4x3_t, uint16_t*, u16);
TEST_STX (vst3, poly16x4x3_t, poly16_t*, p16);
TEST_STX (vst3, float16x4x3_t, float16_t*, f16);
TEST_STX (vst3, bfloat16x4x3_t, bfloat16_t*, bf16);
TEST_STX (vst3, int32x2x3_t, int32_t*, s32);
TEST_STX (vst3, uint32x2x3_t, uint32_t*, u32);
TEST_STX (vst3, float32x2x3_t, float32_t*, f32);
TEST_STX (vst3, int64x1x3_t, int64_t*, s64);
TEST_STX (vst3, uint64x1x3_t, uint64_t*, u64);
TEST_STX (vst3, float64x1x3_t, float64_t*, f64);
TEST_STX (vst3, poly64x1x3_t, poly64_t*, p64);

TEST_STX (vst3q, int8x16x3_t, int8_t*, s8);
TEST_STX (vst3q, uint8x16x3_t, uint8_t*, u8);
TEST_STX (vst3q, poly8x16x3_t, poly8_t*, p8);
TEST_STX (vst3q, int16x8x3_t, int16_t*, s16);
TEST_STX (vst3q, uint16x8x3_t, uint16_t*, u16);
TEST_STX (vst3q, poly16x8x3_t, poly16_t*, p16);
TEST_STX (vst3q, float16x8x3_t, float16_t*, f16);
TEST_STX (vst3q, bfloat16x8x3_t, bfloat16_t*, bf16);
TEST_STX (vst3q, int32x4x3_t, int32_t*, s32);
TEST_STX (vst3q, uint32x4x3_t, uint32_t*, u32);
TEST_STX (vst3q, float32x4x3_t, float32_t*, f32);
TEST_STX (vst3q, int64x2x3_t, int64_t*, s64);
TEST_STX (vst3q, uint64x2x3_t, uint64_t*, u64);
TEST_STX (vst3q, float64x2x3_t, float64_t*, f64);
TEST_STX (vst3q, poly64x2x3_t, poly64_t*, p64);

TEST_STX (vst4, int8x8x4_t, int8_t*, s8);
TEST_STX (vst4, uint8x8x4_t, uint8_t*, u8);
TEST_STX (vst4, poly8x8x4_t, poly8_t*, p8);
TEST_STX (vst4, int16x4x4_t, int16_t*, s16);
TEST_STX (vst4, uint16x4x4_t, uint16_t*, u16);
TEST_STX (vst4, poly16x4x4_t, poly16_t*, p16);
TEST_STX (vst4, float16x4x4_t, float16_t*, f16);
TEST_STX (vst4, bfloat16x4x4_t, bfloat16_t*, bf16);
TEST_STX (vst4, int32x2x4_t, int32_t*, s32);
TEST_STX (vst4, uint32x2x4_t, uint32_t*, u32);
TEST_STX (vst4, float32x2x4_t, float32_t*, f32);
TEST_STX (vst4, int64x1x4_t, int64_t*, s64);
TEST_STX (vst4, uint64x1x4_t, uint64_t*, u64);
TEST_STX (vst4, float64x1x4_t, float64_t*, f64);
TEST_STX (vst4, poly64x1x4_t, poly64_t*, p64);

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

#define TEST_LDX(name, rettype, ptrtype, ts) \
  rettype test_ ## name ## _ ## ts (ptrtype a) \
	{ \
		return name ## _ ## ts (a); \
	}

TEST_LDX (vld2, int8x8x2_t, int8_t*, s8);
TEST_LDX (vld2, uint8x8x2_t, uint8_t*, u8);
TEST_LDX (vld2, poly8x8x2_t, poly8_t*, p8);
TEST_LDX (vld2, int16x4x2_t, int16_t*, s16);
TEST_LDX (vld2, uint16x4x2_t, uint16_t*, u16);
TEST_LDX (vld2, poly16x4x2_t, poly16_t*, p16);
TEST_LDX (vld2, float16x4x2_t, float16_t*, f16);
TEST_LDX (vld2, bfloat16x4x2_t, bfloat16_t*, bf16);
TEST_LDX (vld2, int32x2x2_t, int32_t*, s32);
TEST_LDX (vld2, uint32x2x2_t, uint32_t*, u32);
TEST_LDX (vld2, float32x2x2_t, float32_t*, f32);
TEST_LDX (vld2, int64x1x2_t, int64_t*, s64);
TEST_LDX (vld2, uint64x1x2_t, uint64_t*, u64);
TEST_LDX (vld2, float64x1x2_t, float64_t*, f64);
TEST_LDX (vld2, poly64x1x2_t, poly64_t*, p64);

TEST_LDX (vld2q, int8x16x2_t, int8_t*, s8);
TEST_LDX (vld2q, uint8x16x2_t, uint8_t*, u8);
TEST_LDX (vld2q, poly8x16x2_t, poly8_t*, p8);
TEST_LDX (vld2q, int16x8x2_t, int16_t*, s16);
TEST_LDX (vld2q, uint16x8x2_t, uint16_t*, u16);
TEST_LDX (vld2q, poly16x8x2_t, poly16_t*, p16);
TEST_LDX (vld2q, float16x8x2_t, float16_t*, f16);
TEST_LDX (vld2q, bfloat16x8x2_t, bfloat16_t*, bf16);
TEST_LDX (vld2q, int32x4x2_t, int32_t*, s32);
TEST_LDX (vld2q, uint32x4x2_t, uint32_t*, u32);
TEST_LDX (vld2q, float32x4x2_t, float32_t*, f32);
TEST_LDX (vld2q, int64x2x2_t, int64_t*, s64);
TEST_LDX (vld2q, uint64x2x2_t, uint64_t*, u64);
TEST_LDX (vld2q, float64x2x2_t, float64_t*, f64);
TEST_LDX (vld2q, poly64x2x2_t, poly64_t*, p64);

TEST_LDX (vld3, int8x8x3_t, int8_t*, s8);
TEST_LDX (vld3, uint8x8x3_t, uint8_t*, u8);
TEST_LDX (vld3, poly8x8x3_t, poly8_t*, p8);
TEST_LDX (vld3, int16x4x3_t, int16_t*, s16);
TEST_LDX (vld3, uint16x4x3_t, uint16_t*, u16);
TEST_LDX (vld3, poly16x4x3_t, poly16_t*, p16);
TEST_LDX (vld3, float16x4x3_t, float16_t*, f16);
TEST_LDX (vld3, bfloat16x4x3_t, bfloat16_t*, bf16);
TEST_LDX (vld3, int32x2x3_t, int32_t*, s32);
TEST_LDX (vld3, uint32x2x3_t, uint32_t*, u32);
TEST_LDX (vld3, float32x2x3_t, float32_t*, f32);
TEST_LDX (vld3, int64x1x3_t, int64_t*, s64);
TEST_LDX (vld3, uint64x1x3_t, uint64_t*, u64);
TEST_LDX (vld3, float64x1x3_t, float64_t*, f64);
TEST_LDX (vld3, poly64x1x3_t, poly64_t*, p64);

TEST_LDX (vld3q, int8x16x3_t, int8_t*, s8);
TEST_LDX (vld3q, uint8x16x3_t, uint8_t*, u8);
TEST_LDX (vld3q, poly8x16x3_t, poly8_t*, p8);
TEST_LDX (vld3q, int16x8x3_t, int16_t*, s16);
TEST_LDX (vld3q, uint16x8x3_t, uint16_t*, u16);
TEST_LDX (vld3q, poly16x8x3_t, poly16_t*, p16);
TEST_LDX (vld3q, float16x8x3_t, float16_t*, f16);
TEST_LDX (vld3q, bfloat16x8x3_t, bfloat16_t*, bf16);
TEST_LDX (vld3q, int32x4x3_t, int32_t*, s32);
TEST_LDX (vld3q, uint32x4x3_t, uint32_t*, u32);
TEST_LDX (vld3q, float32x4x3_t, float32_t*, f32);
TEST_LDX (vld3q, int64x2x3_t, int64_t*, s64);
TEST_LDX (vld3q, uint64x2x3_t, uint64_t*, u64);
TEST_LDX (vld3q, float64x2x3_t, float64_t*, f64);
TEST_LDX (vld3q, poly64x2x3_t, poly64_t*, p64);

TEST_LDX (vld4, int8x8x4_t, int8_t*, s8);
TEST_LDX (vld4, uint8x8x4_t, uint8_t*, u8);
TEST_LDX (vld4, poly8x8x4_t, poly8_t*, p8);
TEST_LDX (vld4, int16x4x4_t, int16_t*, s16);
TEST_LDX (vld4, uint16x4x4_t, uint16_t*, u16);
TEST_LDX (vld4, poly16x4x4_t, poly16_t*, p16);
TEST_LDX (vld4, float16x4x4_t, float16_t*, f16);
TEST_LDX (vld4, bfloat16x4x4_t, bfloat16_t*, bf16);
TEST_LDX (vld4, int32x2x4_t, int32_t*, s32);
TEST_LDX (vld4, uint32x2x4_t, uint32_t*, u32);
TEST_LDX (vld4, float32x2x4_t, float32_t*, f32);
TEST_LDX (vld4, int64x1x4_t, int64_t*, s64);
TEST_LDX (vld4, uint64x1x4_t, uint64_t*, u64);
TEST_LDX (vld4, float64x1x4_t, float64_t*, f64);
TEST_LDX (vld4, poly64x1x4_t, poly64_t*, p64);

TEST_LDX (vld4q, int8x16x4_t, int8_t*, s8);
TEST_LDX (vld4q, uint8x16x4_t, uint8_t*, u8);
TEST_LDX (vld4q, poly8x16x4_t, poly8_t*, p8);
TEST_LDX (vld4q, int16x8x4_t, int16_t*, s16);
TEST_LDX (vld4q, uint16x8x4_t, uint16_t*, u16);
TEST_LDX (vld4q, poly16x8x4_t, poly16_t*, p16);
TEST_LDX (vld4q, float16x8x4_t, float16_t*, f16);
TEST_LDX (vld4q, bfloat16x8x4_t, bfloat16_t*, bf16);
TEST_LDX (vld4q, int32x4x4_t, int32_t*, s32);
TEST_LDX (vld4q, uint32x4x4_t, uint32_t*, u32);
TEST_LDX (vld4q, float32x4x4_t, float32_t*, f32);
TEST_LDX (vld4q, int64x2x4_t, int64_t*, s64);
TEST_LDX (vld4q, uint64x2x4_t, uint64_t*, u64);
TEST_LDX (vld4q, float64x2x4_t, float64_t*, f64);
TEST_LDX (vld4q, poly64x2x4_t, poly64_t*, p64);

#define TEST_STX_LANE(name, tbltype, ptrtype, ts) \
  void test_ ## name ## _ ## ts (ptrtype a, tbltype b) \
	{ \
		name ## _ ## ts (a, b, 0); \
	}

TEST_STX_LANE (vst2_lane, int8x8x2_t, int8_t*, s8);
TEST_STX_LANE (vst2_lane, uint8x8x2_t, uint8_t*, u8);
TEST_STX_LANE (vst2_lane, poly8x8x2_t, poly8_t*, p8);
TEST_STX_LANE (vst2_lane, int16x4x2_t, int16_t*, s16);
TEST_STX_LANE (vst2_lane, uint16x4x2_t, uint16_t*, u16);
TEST_STX_LANE (vst2_lane, poly16x4x2_t, poly16_t*, p16);
TEST_STX_LANE (vst2_lane, float16x4x2_t, float16_t*, f16);
TEST_STX_LANE (vst2_lane, bfloat16x4x2_t, bfloat16_t*, bf16);
TEST_STX_LANE (vst2_lane, int32x2x2_t, int32_t*, s32);
TEST_STX_LANE (vst2_lane, uint32x2x2_t, uint32_t*, u32);
TEST_STX_LANE (vst2_lane, float32x2x2_t, float32_t*, f32);
TEST_STX_LANE (vst2_lane, int64x1x2_t, int64_t*, s64);
TEST_STX_LANE (vst2_lane, uint64x1x2_t, uint64_t*, u64);
TEST_STX_LANE (vst2_lane, float64x1x2_t, float64_t*, f64);
TEST_STX_LANE (vst2_lane, poly64x1x2_t, poly64_t*, p64);

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

TEST_STX_LANE (vst3_lane, int8x8x3_t, int8_t*, s8);
TEST_STX_LANE (vst3_lane, uint8x8x3_t, uint8_t*, u8);
TEST_STX_LANE (vst3_lane, poly8x8x3_t, poly8_t*, p8);
TEST_STX_LANE (vst3_lane, int16x4x3_t, int16_t*, s16);
TEST_STX_LANE (vst3_lane, uint16x4x3_t, uint16_t*, u16);
TEST_STX_LANE (vst3_lane, poly16x4x3_t, poly16_t*, p16);
TEST_STX_LANE (vst3_lane, float16x4x3_t, float16_t*, f16);
TEST_STX_LANE (vst3_lane, bfloat16x4x3_t, bfloat16_t*, bf16);
TEST_STX_LANE (vst3_lane, int32x2x3_t, int32_t*, s32);
TEST_STX_LANE (vst3_lane, uint32x2x3_t, uint32_t*, u32);
TEST_STX_LANE (vst3_lane, float32x2x3_t, float32_t*, f32);
TEST_STX_LANE (vst3_lane, int64x1x3_t, int64_t*, s64);
TEST_STX_LANE (vst3_lane, uint64x1x3_t, uint64_t*, u64);
TEST_STX_LANE (vst3_lane, float64x1x3_t, float64_t*, f64);
TEST_STX_LANE (vst3_lane, poly64x1x3_t, poly64_t*, p64);

TEST_STX_LANE (vst3q_lane, int8x16x3_t, int8_t*, s8);
TEST_STX_LANE (vst3q_lane, uint8x16x3_t, uint8_t*, u8);
TEST_STX_LANE (vst3q_lane, poly8x16x3_t, poly8_t*, p8);
TEST_STX_LANE (vst3q_lane, int16x8x3_t, int16_t*, s16);
TEST_STX_LANE (vst3q_lane, uint16x8x3_t, uint16_t*, u16);
TEST_STX_LANE (vst3q_lane, poly16x8x3_t, poly16_t*, p16);
TEST_STX_LANE (vst3q_lane, float16x8x3_t, float16_t*, f16);
TEST_STX_LANE (vst3q_lane, bfloat16x8x3_t, bfloat16_t*, bf16);
TEST_STX_LANE (vst3q_lane, int32x4x3_t, int32_t*, s32);
TEST_STX_LANE (vst3q_lane, uint32x4x3_t, uint32_t*, u32);
TEST_STX_LANE (vst3q_lane, float32x4x3_t, float32_t*, f32);
TEST_STX_LANE (vst3q_lane, int64x2x3_t, int64_t*, s64);
TEST_STX_LANE (vst3q_lane, uint64x2x3_t, uint64_t*, u64);
TEST_STX_LANE (vst3q_lane, float64x2x3_t, float64_t*, f64);
TEST_STX_LANE (vst3q_lane, poly64x2x3_t, poly64_t*, p64);

TEST_STX_LANE (vst4_lane, int8x8x4_t, int8_t*, s8);
TEST_STX_LANE (vst4_lane, uint8x8x4_t, uint8_t*, u8);
TEST_STX_LANE (vst4_lane, poly8x8x4_t, poly8_t*, p8);
TEST_STX_LANE (vst4_lane, int16x4x4_t, int16_t*, s16);
TEST_STX_LANE (vst4_lane, uint16x4x4_t, uint16_t*, u16);
TEST_STX_LANE (vst4_lane, poly16x4x4_t, poly16_t*, p16);
TEST_STX_LANE (vst4_lane, float16x4x4_t, float16_t*, f16);
TEST_STX_LANE (vst4_lane, bfloat16x4x4_t, bfloat16_t*, bf16);
TEST_STX_LANE (vst4_lane, int32x2x4_t, int32_t*, s32);
TEST_STX_LANE (vst4_lane, uint32x2x4_t, uint32_t*, u32);
TEST_STX_LANE (vst4_lane, float32x2x4_t, float32_t*, f32);
TEST_STX_LANE (vst4_lane, int64x1x4_t, int64_t*, s64);
TEST_STX_LANE (vst4_lane, uint64x1x4_t, uint64_t*, u64);
TEST_STX_LANE (vst4_lane, float64x1x4_t, float64_t*, f64);
TEST_STX_LANE (vst4_lane, poly64x1x4_t, poly64_t*, p64);

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

#define TEST_LDX_LANE(name, rettype, ptrtype, ts) \
  rettype test_ ## name ## _ ## ts (ptrtype a, rettype b) \
	{ \
		return name ## _ ## ts (a, b, 0); \
	}

TEST_LDX_LANE (vld2_lane, int8x8x2_t, int8_t*, s8);
TEST_LDX_LANE (vld2_lane, uint8x8x2_t, uint8_t*, u8);
TEST_LDX_LANE (vld2_lane, poly8x8x2_t, poly8_t*, p8);
TEST_LDX_LANE (vld2_lane, int16x4x2_t, int16_t*, s16);
TEST_LDX_LANE (vld2_lane, uint16x4x2_t, uint16_t*, u16);
TEST_LDX_LANE (vld2_lane, poly16x4x2_t, poly16_t*, p16);
TEST_LDX_LANE (vld2_lane, float16x4x2_t, float16_t*, f16);
TEST_LDX_LANE (vld2_lane, bfloat16x4x2_t, bfloat16_t*, bf16);
TEST_LDX_LANE (vld2_lane, int32x2x2_t, int32_t*, s32);
TEST_LDX_LANE (vld2_lane, uint32x2x2_t, uint32_t*, u32);
TEST_LDX_LANE (vld2_lane, float32x2x2_t, float32_t*, f32);
TEST_LDX_LANE (vld2_lane, int64x1x2_t, int64_t*, s64);
TEST_LDX_LANE (vld2_lane, uint64x1x2_t, uint64_t*, u64);
TEST_LDX_LANE (vld2_lane, float64x1x2_t, float64_t*, f64);
TEST_LDX_LANE (vld2_lane, poly64x1x2_t, poly64_t*, p64);

TEST_LDX_LANE (vld2q_lane, int8x16x2_t, int8_t*, s8);
TEST_LDX_LANE (vld2q_lane, uint8x16x2_t, uint8_t*, u8);
TEST_LDX_LANE (vld2q_lane, poly8x16x2_t, poly8_t*, p8);
TEST_LDX_LANE (vld2q_lane, int16x8x2_t, int16_t*, s16);
TEST_LDX_LANE (vld2q_lane, uint16x8x2_t, uint16_t*, u16);
TEST_LDX_LANE (vld2q_lane, poly16x8x2_t, poly16_t*, p16);
TEST_LDX_LANE (vld2q_lane, float16x8x2_t, float16_t*, f16);
TEST_LDX_LANE (vld2q_lane, bfloat16x8x2_t, bfloat16_t*, bf16);
TEST_LDX_LANE (vld2q_lane, int32x4x2_t, int32_t*, s32);
TEST_LDX_LANE (vld2q_lane, uint32x4x2_t, uint32_t*, u32);
TEST_LDX_LANE (vld2q_lane, float32x4x2_t, float32_t*, f32);
TEST_LDX_LANE (vld2q_lane, int64x2x2_t, int64_t*, s64);
TEST_LDX_LANE (vld2q_lane, uint64x2x2_t, uint64_t*, u64);
TEST_LDX_LANE (vld2q_lane, float64x2x2_t, float64_t*, f64);
TEST_LDX_LANE (vld2q_lane, poly64x2x2_t, poly64_t*, p64);

TEST_LDX_LANE (vld3_lane, int8x8x3_t, int8_t*, s8);
TEST_LDX_LANE (vld3_lane, uint8x8x3_t, uint8_t*, u8);
TEST_LDX_LANE (vld3_lane, poly8x8x3_t, poly8_t*, p8);
TEST_LDX_LANE (vld3_lane, int16x4x3_t, int16_t*, s16);
TEST_LDX_LANE (vld3_lane, uint16x4x3_t, uint16_t*, u16);
TEST_LDX_LANE (vld3_lane, poly16x4x3_t, poly16_t*, p16);
TEST_LDX_LANE (vld3_lane, float16x4x3_t, float16_t*, f16);
TEST_LDX_LANE (vld3_lane, bfloat16x4x3_t, bfloat16_t*, bf16);
TEST_LDX_LANE (vld3_lane, int32x2x3_t, int32_t*, s32);
TEST_LDX_LANE (vld3_lane, uint32x2x3_t, uint32_t*, u32);
TEST_LDX_LANE (vld3_lane, float32x2x3_t, float32_t*, f32);
TEST_LDX_LANE (vld3_lane, int64x1x3_t, int64_t*, s64);
TEST_LDX_LANE (vld3_lane, uint64x1x3_t, uint64_t*, u64);
TEST_LDX_LANE (vld3_lane, float64x1x3_t, float64_t*, f64);
TEST_LDX_LANE (vld3_lane, poly64x1x3_t, poly64_t*, p64);

TEST_LDX_LANE (vld3q_lane, int8x16x3_t, int8_t*, s8);
TEST_LDX_LANE (vld3q_lane, uint8x16x3_t, uint8_t*, u8);
TEST_LDX_LANE (vld3q_lane, poly8x16x3_t, poly8_t*, p8);
TEST_LDX_LANE (vld3q_lane, int16x8x3_t, int16_t*, s16);
TEST_LDX_LANE (vld3q_lane, uint16x8x3_t, uint16_t*, u16);
TEST_LDX_LANE (vld3q_lane, poly16x8x3_t, poly16_t*, p16);
TEST_LDX_LANE (vld3q_lane, float16x8x3_t, float16_t*, f16);
TEST_LDX_LANE (vld3q_lane, bfloat16x8x3_t, bfloat16_t*, bf16);
TEST_LDX_LANE (vld3q_lane, int32x4x3_t, int32_t*, s32);
TEST_LDX_LANE (vld3q_lane, uint32x4x3_t, uint32_t*, u32);
TEST_LDX_LANE (vld3q_lane, float32x4x3_t, float32_t*, f32);
TEST_LDX_LANE (vld3q_lane, int64x2x3_t, int64_t*, s64);
TEST_LDX_LANE (vld3q_lane, uint64x2x3_t, uint64_t*, u64);
TEST_LDX_LANE (vld3q_lane, float64x2x3_t, float64_t*, f64);
TEST_LDX_LANE (vld3q_lane, poly64x2x3_t, poly64_t*, p64);

TEST_LDX_LANE (vld4_lane, int8x8x4_t, int8_t*, s8);
TEST_LDX_LANE (vld4_lane, uint8x8x4_t, uint8_t*, u8);
TEST_LDX_LANE (vld4_lane, poly8x8x4_t, poly8_t*, p8);
TEST_LDX_LANE (vld4_lane, int16x4x4_t, int16_t*, s16);
TEST_LDX_LANE (vld4_lane, uint16x4x4_t, uint16_t*, u16);
TEST_LDX_LANE (vld4_lane, poly16x4x4_t, poly16_t*, p16);
TEST_LDX_LANE (vld4_lane, float16x4x4_t, float16_t*, f16);
TEST_LDX_LANE (vld4_lane, bfloat16x4x4_t, bfloat16_t*, bf16);
TEST_LDX_LANE (vld4_lane, int32x2x4_t, int32_t*, s32);
TEST_LDX_LANE (vld4_lane, uint32x2x4_t, uint32_t*, u32);
TEST_LDX_LANE (vld4_lane, float32x2x4_t, float32_t*, f32);
TEST_LDX_LANE (vld4_lane, int64x1x4_t, int64_t*, s64);
TEST_LDX_LANE (vld4_lane, uint64x1x4_t, uint64_t*, u64);
TEST_LDX_LANE (vld4_lane, float64x1x4_t, float64_t*, f64);
TEST_LDX_LANE (vld4_lane, poly64x1x4_t, poly64_t*, p64);

TEST_LDX_LANE (vld4q_lane, int8x16x4_t, int8_t*, s8);
TEST_LDX_LANE (vld4q_lane, uint8x16x4_t, uint8_t*, u8);
TEST_LDX_LANE (vld4q_lane, poly8x16x4_t, poly8_t*, p8);
TEST_LDX_LANE (vld4q_lane, int16x8x4_t, int16_t*, s16);
TEST_LDX_LANE (vld4q_lane, uint16x8x4_t, uint16_t*, u16);
TEST_LDX_LANE (vld4q_lane, poly16x8x4_t, poly16_t*, p16);
TEST_LDX_LANE (vld4q_lane, float16x8x4_t, float16_t*, f16);
TEST_LDX_LANE (vld4q_lane, bfloat16x8x4_t, bfloat16_t*, bf16);
TEST_LDX_LANE (vld4q_lane, int32x4x4_t, int32_t*, s32);
TEST_LDX_LANE (vld4q_lane, uint32x4x4_t, uint32_t*, u32);
TEST_LDX_LANE (vld4q_lane, float32x4x4_t, float32_t*, f32);
TEST_LDX_LANE (vld4q_lane, int64x2x4_t, int64_t*, s64);
TEST_LDX_LANE (vld4q_lane, uint64x2x4_t, uint64_t*, u64);
TEST_LDX_LANE (vld4q_lane, float64x2x4_t, float64_t*, f64);
TEST_LDX_LANE (vld4q_lane, poly64x2x4_t, poly64_t*, p64);

#define TEST_ST1xN(name, tbltype, ptrtype, ts, xn) \
  void test_ ## name ## _ ## ts ## _ ## xn (ptrtype a, tbltype b) \
	{ \
		name ## _ ## ts ## _ ## xn (a, b); \
	}

TEST_ST1xN (vst1, int8x8x2_t, int8_t*, s8, x2);
TEST_ST1xN (vst1, uint8x8x2_t, uint8_t*, u8, x2);
TEST_ST1xN (vst1, poly8x8x2_t, poly8_t*, p8, x2);
TEST_ST1xN (vst1, int16x4x2_t, int16_t*, s16, x2);
TEST_ST1xN (vst1, uint16x4x2_t, uint16_t*, u16, x2);
TEST_ST1xN (vst1, poly16x4x2_t, poly16_t*, p16, x2);
TEST_ST1xN (vst1, float16x4x2_t, float16_t*, f16, x2);
TEST_ST1xN (vst1, bfloat16x4x2_t, bfloat16_t*, bf16, x2);
TEST_ST1xN (vst1, int32x2x2_t, int32_t*, s32, x2);
TEST_ST1xN (vst1, uint32x2x2_t, uint32_t*, u32, x2);
TEST_ST1xN (vst1, float32x2x2_t, float32_t*, f32, x2);
TEST_ST1xN (vst1, int64x1x2_t, int64_t*, s64, x2);
TEST_ST1xN (vst1, uint64x1x2_t, uint64_t*, u64, x2);
TEST_ST1xN (vst1, poly64x1x2_t, poly64_t*, p64, x2);
TEST_ST1xN (vst1, float64x1x2_t, float64_t*, f64, x2);

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

TEST_ST1xN (vst1, int8x8x3_t, int8_t*, s8, x3);
TEST_ST1xN (vst1, uint8x8x3_t, uint8_t*, u8, x3);
TEST_ST1xN (vst1, poly8x8x3_t, poly8_t*, p8, x3);
TEST_ST1xN (vst1, int16x4x3_t, int16_t*, s16, x3);
TEST_ST1xN (vst1, uint16x4x3_t, uint16_t*, u16, x3);
TEST_ST1xN (vst1, poly16x4x3_t, poly16_t*, p16, x3);
TEST_ST1xN (vst1, float16x4x3_t, float16_t*, f16, x3);
TEST_ST1xN (vst1, bfloat16x4x3_t, bfloat16_t*, bf16, x3);
TEST_ST1xN (vst1, int32x2x3_t, int32_t*, s32, x3);
TEST_ST1xN (vst1, uint32x2x3_t, uint32_t*, u32, x3);
TEST_ST1xN (vst1, float32x2x3_t, float32_t*, f32, x3);
TEST_ST1xN (vst1, int64x1x3_t, int64_t*, s64, x3);
TEST_ST1xN (vst1, uint64x1x3_t, uint64_t*, u64, x3);
TEST_ST1xN (vst1, poly64x1x3_t, poly64_t*, p64, x3);
TEST_ST1xN (vst1, float64x1x3_t, float64_t*, f64, x3);

TEST_ST1xN (vst1q, int8x16x3_t, int8_t*, s8, x3);
TEST_ST1xN (vst1q, uint8x16x3_t, uint8_t*, u8, x3);
TEST_ST1xN (vst1q, poly8x16x3_t, poly8_t*, p8, x3);
TEST_ST1xN (vst1q, int16x8x3_t, int16_t*, s16, x3);
TEST_ST1xN (vst1q, uint16x8x3_t, uint16_t*, u16, x3);
TEST_ST1xN (vst1q, poly16x8x3_t, poly16_t*, p16, x3);
TEST_ST1xN (vst1q, float16x8x3_t, float16_t*, f16, x3);
TEST_ST1xN (vst1q, bfloat16x8x3_t, bfloat16_t*, bf16, x3);
TEST_ST1xN (vst1q, int32x4x3_t, int32_t*, s32, x3);
TEST_ST1xN (vst1q, uint32x4x3_t, uint32_t*, u32, x3);
TEST_ST1xN (vst1q, float32x4x3_t, float32_t*, f32, x3);
TEST_ST1xN (vst1q, int64x2x3_t, int64_t*, s64, x3);
TEST_ST1xN (vst1q, uint64x2x3_t, uint64_t*, u64, x3);
TEST_ST1xN (vst1q, poly64x2x3_t, poly64_t*, p64, x3);
TEST_ST1xN (vst1q, float64x2x3_t, float64_t*, f64, x3);

TEST_ST1xN (vst1, int8x8x4_t, int8_t*, s8, x4);
TEST_ST1xN (vst1, uint8x8x4_t, uint8_t*, u8, x4);
TEST_ST1xN (vst1, poly8x8x4_t, poly8_t*, p8, x4);
TEST_ST1xN (vst1, int16x4x4_t, int16_t*, s16, x4);
TEST_ST1xN (vst1, uint16x4x4_t, uint16_t*, u16, x4);
TEST_ST1xN (vst1, poly16x4x4_t, poly16_t*, p16, x4);
TEST_ST1xN (vst1, float16x4x4_t, float16_t*, f16, x4);
TEST_ST1xN (vst1, bfloat16x4x4_t, bfloat16_t*, bf16, x4);
TEST_ST1xN (vst1, int32x2x4_t, int32_t*, s32, x4);
TEST_ST1xN (vst1, uint32x2x4_t, uint32_t*, u32, x4);
TEST_ST1xN (vst1, float32x2x4_t, float32_t*, f32, x4);
TEST_ST1xN (vst1, int64x1x4_t, int64_t*, s64, x4);
TEST_ST1xN (vst1, uint64x1x4_t, uint64_t*, u64, x4);
TEST_ST1xN (vst1, poly64x1x4_t, poly64_t*, p64, x4);
TEST_ST1xN (vst1, float64x1x4_t, float64_t*, f64, x4);

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

#define TEST_LD1xN(name, rettype, ptrtype, ts, xn) \
  rettype test_ ## name ## _ ## ts ## _ ## xn (ptrtype a) \
	{ \
		return name ## _ ## ts ## _ ## xn (a); \
	}

TEST_LD1xN (vld1, int8x8x2_t, int8_t*, s8, x2);
TEST_LD1xN (vld1, uint8x8x2_t, uint8_t*, u8, x2);
TEST_LD1xN (vld1, poly8x8x2_t, poly8_t*, p8, x2);
TEST_LD1xN (vld1, int16x4x2_t, int16_t*, s16, x2);
TEST_LD1xN (vld1, uint16x4x2_t, uint16_t*, u16, x2);
TEST_LD1xN (vld1, poly16x4x2_t, poly16_t*, p16, x2);
TEST_LD1xN (vld1, float16x4x2_t, float16_t*, f16, x2);
TEST_LD1xN (vld1, bfloat16x4x2_t, bfloat16_t*, bf16, x2);
TEST_LD1xN (vld1, int32x2x2_t, int32_t*, s32, x2);
TEST_LD1xN (vld1, uint32x2x2_t, uint32_t*, u32, x2);
TEST_LD1xN (vld1, float32x2x2_t, float32_t*, f32, x2);
TEST_LD1xN (vld1, int64x1x2_t, int64_t*, s64, x2);
TEST_LD1xN (vld1, uint64x1x2_t, uint64_t*, u64, x2);
TEST_LD1xN (vld1, poly64x1x2_t, poly64_t*, p64, x2);
TEST_LD1xN (vld1, float64x1x2_t, float64_t*, f64, x2);

TEST_LD1xN (vld1q, int8x16x2_t, int8_t*, s8, x2);
TEST_LD1xN (vld1q, uint8x16x2_t, uint8_t*, u8, x2);
TEST_LD1xN (vld1q, poly8x16x2_t, poly8_t*, p8, x2);
TEST_LD1xN (vld1q, int16x8x2_t, int16_t*, s16, x2);
TEST_LD1xN (vld1q, uint16x8x2_t, uint16_t*, u16, x2);
TEST_LD1xN (vld1q, poly16x8x2_t, poly16_t*, p16, x2);
TEST_LD1xN (vld1q, float16x8x2_t, float16_t*, f16, x2);
TEST_LD1xN (vld1q, bfloat16x8x2_t, bfloat16_t*, bf16, x2);
TEST_LD1xN (vld1q, int32x4x2_t, int32_t*, s32, x2);
TEST_LD1xN (vld1q, uint32x4x2_t, uint32_t*, u32, x2);
TEST_LD1xN (vld1q, float32x4x2_t, float32_t*, f32, x2);
TEST_LD1xN (vld1q, int64x2x2_t, int64_t*, s64, x2);
TEST_LD1xN (vld1q, uint64x2x2_t, uint64_t*, u64, x2);
TEST_LD1xN (vld1q, poly64x2x2_t, poly64_t*, p64, x2);
TEST_LD1xN (vld1q, float64x2x2_t, float64_t*, f64, x2);

TEST_LD1xN (vld1, int8x8x3_t, int8_t*, s8, x3);
TEST_LD1xN (vld1, uint8x8x3_t, uint8_t*, u8, x3);
TEST_LD1xN (vld1, poly8x8x3_t, poly8_t*, p8, x3);
TEST_LD1xN (vld1, int16x4x3_t, int16_t*, s16, x3);
TEST_LD1xN (vld1, uint16x4x3_t, uint16_t*, u16, x3);
TEST_LD1xN (vld1, poly16x4x3_t, poly16_t*, p16, x3);
TEST_LD1xN (vld1, float16x4x3_t, float16_t*, f16, x3);
TEST_LD1xN (vld1, bfloat16x4x3_t, bfloat16_t*, bf16, x3);
TEST_LD1xN (vld1, int32x2x3_t, int32_t*, s32, x3);
TEST_LD1xN (vld1, uint32x2x3_t, uint32_t*, u32, x3);
TEST_LD1xN (vld1, float32x2x3_t, float32_t*, f32, x3);
TEST_LD1xN (vld1, int64x1x3_t, int64_t*, s64, x3);
TEST_LD1xN (vld1, uint64x1x3_t, uint64_t*, u64, x3);
TEST_LD1xN (vld1, poly64x1x3_t, poly64_t*, p64, x3);
TEST_LD1xN (vld1, float64x1x3_t, float64_t*, f64, x3);

TEST_LD1xN (vld1q, int8x16x3_t, int8_t*, s8, x3);
TEST_LD1xN (vld1q, uint8x16x3_t, uint8_t*, u8, x3);
TEST_LD1xN (vld1q, poly8x16x3_t, poly8_t*, p8, x3);
TEST_LD1xN (vld1q, int16x8x3_t, int16_t*, s16, x3);
TEST_LD1xN (vld1q, uint16x8x3_t, uint16_t*, u16, x3);
TEST_LD1xN (vld1q, poly16x8x3_t, poly16_t*, p16, x3);
TEST_LD1xN (vld1q, float16x8x3_t, float16_t*, f16, x3);
TEST_LD1xN (vld1q, bfloat16x8x3_t, bfloat16_t*, bf16, x3);
TEST_LD1xN (vld1q, int32x4x3_t, int32_t*, s32, x3);
TEST_LD1xN (vld1q, uint32x4x3_t, uint32_t*, u32, x3);
TEST_LD1xN (vld1q, float32x4x3_t, float32_t*, f32, x3);
TEST_LD1xN (vld1q, int64x2x3_t, int64_t*, s64, x3);
TEST_LD1xN (vld1q, uint64x2x3_t, uint64_t*, u64, x3);
TEST_LD1xN (vld1q, poly64x2x3_t, poly64_t*, p64, x3);
TEST_LD1xN (vld1q, float64x2x3_t, float64_t*, f64, x3);

TEST_LD1xN (vld1, int8x8x4_t, int8_t*, s8, x4);
TEST_LD1xN (vld1, uint8x8x4_t, uint8_t*, u8, x4);
TEST_LD1xN (vld1, poly8x8x4_t, poly8_t*, p8, x4);
TEST_LD1xN (vld1, int16x4x4_t, int16_t*, s16, x4);
TEST_LD1xN (vld1, uint16x4x4_t, uint16_t*, u16, x4);
TEST_LD1xN (vld1, poly16x4x4_t, poly16_t*, p16, x4);
TEST_LD1xN (vld1, float16x4x4_t, float16_t*, f16, x4);
TEST_LD1xN (vld1, bfloat16x4x4_t, bfloat16_t*, bf16, x4);
TEST_LD1xN (vld1, int32x2x4_t, int32_t*, s32, x4);
TEST_LD1xN (vld1, uint32x2x4_t, uint32_t*, u32, x4);
TEST_LD1xN (vld1, float32x2x4_t, float32_t*, f32, x4);
TEST_LD1xN (vld1, int64x1x4_t, int64_t*, s64, x4);
TEST_LD1xN (vld1, uint64x1x4_t, uint64_t*, u64, x4);
TEST_LD1xN (vld1, poly64x1x4_t, poly64_t*, p64, x4);
TEST_LD1xN (vld1, float64x1x4_t, float64_t*, f64, x4);

TEST_LD1xN (vld1q, int8x16x4_t, int8_t*, s8, x4);
TEST_LD1xN (vld1q, uint8x16x4_t, uint8_t*, u8, x4);
TEST_LD1xN (vld1q, poly8x16x4_t, poly8_t*, p8, x4);
TEST_LD1xN (vld1q, int16x8x4_t, int16_t*, s16, x4);
TEST_LD1xN (vld1q, uint16x8x4_t, uint16_t*, u16, x4);
TEST_LD1xN (vld1q, poly16x8x4_t, poly16_t*, p16, x4);
TEST_LD1xN (vld1q, float16x8x4_t, float16_t*, f16, x4);
TEST_LD1xN (vld1q, bfloat16x8x4_t, bfloat16_t*, bf16, x4);
TEST_LD1xN (vld1q, int32x4x4_t, int32_t*, s32, x4);
TEST_LD1xN (vld1q, uint32x4x4_t, uint32_t*, u32, x4);
TEST_LD1xN (vld1q, float32x4x4_t, float32_t*, f32, x4);
TEST_LD1xN (vld1q, int64x2x4_t, int64_t*, s64, x4);
TEST_LD1xN (vld1q, uint64x2x4_t, uint64_t*, u64, x4);
TEST_LD1xN (vld1q, poly64x2x4_t, poly64_t*, p64, x4);
TEST_LD1xN (vld1q, float64x2x4_t, float64_t*, f64, x4);

/* { dg-final { scan-assembler-not "mov\\t" { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-not "ldr\\t" { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-not "str\\t" { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-not "sp" { target aarch64_little_endian } } } */

/* { dg-final { scan-assembler-times "tbl\\t" 18} }  */
/* { dg-final { scan-assembler-times "tbx\\t" 18} }  */
/* { dg-final { scan-assembler-times "st4\\t" 56} }  */
/* { dg-final { scan-assembler-times "st3\\t" 56} }  */
/* { dg-final { scan-assembler-times "st2\\t" 56} }  */
/* { dg-final { scan-assembler-times "st1\\t" 102} }  */
/* { dg-final { scan-assembler-times "ld4\\t" 56} }  */
/* { dg-final { scan-assembler-times "ld3\\t" 56} }  */
/* { dg-final { scan-assembler-times "ld2\\t" 56} }  */
/* { dg-final { scan-assembler-times "ld1\\t" 102} }  */
