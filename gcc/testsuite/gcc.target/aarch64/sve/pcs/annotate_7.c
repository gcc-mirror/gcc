/* { dg-do compile } */

#include <arm_sve.h>

void fn_s8 (float d0, float d1, float d2, float d3,
	    float d4, float d5, float d6, float d7, svint8_t x) {}
void fn_s16 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svint16_t x) {}
void fn_s32 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svint32_t x) {}
void fn_s64 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svint64_t x) {}
void fn_u8 (float d0, float d1, float d2, float d3,
	    float d4, float d5, float d6, float d7, svuint8_t x) {}
void fn_u16 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svuint16_t x) {}
void fn_u32 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svuint32_t x) {}
void fn_u64 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svuint64_t x) {}
void fn_bf16 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svbfloat16_t x) {}
void fn_f16 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svfloat16_t x) {}
void fn_f32 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svfloat32_t x) {}
void fn_f64 (float d0, float d1, float d2, float d3,
	     float d4, float d5, float d6, float d7, svfloat64_t x) {}

void fn_s8x2 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svint8x2_t x) {}
void fn_s16x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint16x2_t x) {}
void fn_s32x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint32x2_t x) {}
void fn_s64x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint64x2_t x) {}
void fn_u8x2 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svuint8x2_t x) {}
void fn_u16x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint16x2_t x) {}
void fn_u32x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint32x2_t x) {}
void fn_u64x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint64x2_t x) {}
void fn_bf16x2 (float d0, float d1, float d2, float d3,
		float d4, float d5, float d6, float d7, svbfloat16x2_t x) {}
void fn_f16x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat16x2_t x) {}
void fn_f32x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat32x2_t x) {}
void fn_f64x2 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat64x2_t x) {}

void fn_s8x3 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svint8x3_t x) {}
void fn_s16x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint16x3_t x) {}
void fn_s32x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint32x3_t x) {}
void fn_s64x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint64x3_t x) {}
void fn_u8x3 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svuint8x3_t x) {}
void fn_u16x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint16x3_t x) {}
void fn_u32x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint32x3_t x) {}
void fn_u64x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint64x3_t x) {}
void fn_bf16x3 (float d0, float d1, float d2, float d3,
		float d4, float d5, float d6, float d7, svbfloat16x3_t x) {}
void fn_f16x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat16x3_t x) {}
void fn_f32x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat32x3_t x) {}
void fn_f64x3 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat64x3_t x) {}

void fn_s8x4 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svint8x4_t x) {}
void fn_s16x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint16x4_t x) {}
void fn_s32x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint32x4_t x) {}
void fn_s64x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svint64x4_t x) {}
void fn_u8x4 (float d0, float d1, float d2, float d3,
	      float d4, float d5, float d6, float d7, svuint8x4_t x) {}
void fn_u16x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint16x4_t x) {}
void fn_u32x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint32x4_t x) {}
void fn_u64x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svuint64x4_t x) {}
void fn_bf16x4 (float d0, float d1, float d2, float d3,
		float d4, float d5, float d6, float d7, svbfloat16x4_t x) {}
void fn_f16x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat16x4_t x) {}
void fn_f32x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat32x4_t x) {}
void fn_f64x4 (float d0, float d1, float d2, float d3,
	       float d4, float d5, float d6, float d7, svfloat64x4_t x) {}

/* { dg-final { scan-assembler-not {\t\.variant_pcs\t\n} } } */
