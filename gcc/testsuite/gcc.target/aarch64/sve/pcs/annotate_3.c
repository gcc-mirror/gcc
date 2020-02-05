/* { dg-do compile } */

#include <arm_sve.h>

void fn_s8 (float d0, float d1, float d2, float d3, svint8_t x) {}
void fn_s16 (float d0, float d1, float d2, float d3, svint16_t x) {}
void fn_s32 (float d0, float d1, float d2, float d3, svint32_t x) {}
void fn_s64 (float d0, float d1, float d2, float d3, svint64_t x) {}
void fn_u8 (float d0, float d1, float d2, float d3, svuint8_t x) {}
void fn_u16 (float d0, float d1, float d2, float d3, svuint16_t x) {}
void fn_u32 (float d0, float d1, float d2, float d3, svuint32_t x) {}
void fn_u64 (float d0, float d1, float d2, float d3, svuint64_t x) {}
void fn_bf16 (float d0, float d1, float d2, float d3, svbfloat16_t x) {}
void fn_f16 (float d0, float d1, float d2, float d3, svfloat16_t x) {}
void fn_f32 (float d0, float d1, float d2, float d3, svfloat32_t x) {}
void fn_f64 (float d0, float d1, float d2, float d3, svfloat64_t x) {}

void fn_s8x2 (float d0, float d1, float d2, float d3, svint8x2_t x) {}
void fn_s16x2 (float d0, float d1, float d2, float d3, svint16x2_t x) {}
void fn_s32x2 (float d0, float d1, float d2, float d3, svint32x2_t x) {}
void fn_s64x2 (float d0, float d1, float d2, float d3, svint64x2_t x) {}
void fn_u8x2 (float d0, float d1, float d2, float d3, svuint8x2_t x) {}
void fn_u16x2 (float d0, float d1, float d2, float d3, svuint16x2_t x) {}
void fn_u32x2 (float d0, float d1, float d2, float d3, svuint32x2_t x) {}
void fn_u64x2 (float d0, float d1, float d2, float d3, svuint64x2_t x) {}
void fn_bf16x2 (float d0, float d1, float d2, float d3, svbfloat16x2_t x) {}
void fn_f16x2 (float d0, float d1, float d2, float d3, svfloat16x2_t x) {}
void fn_f32x2 (float d0, float d1, float d2, float d3, svfloat32x2_t x) {}
void fn_f64x2 (float d0, float d1, float d2, float d3, svfloat64x2_t x) {}

void fn_s8x3 (float d0, float d1, float d2, float d3, svint8x3_t x) {}
void fn_s16x3 (float d0, float d1, float d2, float d3, svint16x3_t x) {}
void fn_s32x3 (float d0, float d1, float d2, float d3, svint32x3_t x) {}
void fn_s64x3 (float d0, float d1, float d2, float d3, svint64x3_t x) {}
void fn_u8x3 (float d0, float d1, float d2, float d3, svuint8x3_t x) {}
void fn_u16x3 (float d0, float d1, float d2, float d3, svuint16x3_t x) {}
void fn_u32x3 (float d0, float d1, float d2, float d3, svuint32x3_t x) {}
void fn_u64x3 (float d0, float d1, float d2, float d3, svuint64x3_t x) {}
void fn_bf16x3 (float d0, float d1, float d2, float d3, svbfloat16x3_t x) {}
void fn_f16x3 (float d0, float d1, float d2, float d3, svfloat16x3_t x) {}
void fn_f32x3 (float d0, float d1, float d2, float d3, svfloat32x3_t x) {}
void fn_f64x3 (float d0, float d1, float d2, float d3, svfloat64x3_t x) {}

void fn_s8x4 (float d0, float d1, float d2, float d3, svint8x4_t x) {}
void fn_s16x4 (float d0, float d1, float d2, float d3, svint16x4_t x) {}
void fn_s32x4 (float d0, float d1, float d2, float d3, svint32x4_t x) {}
void fn_s64x4 (float d0, float d1, float d2, float d3, svint64x4_t x) {}
void fn_u8x4 (float d0, float d1, float d2, float d3, svuint8x4_t x) {}
void fn_u16x4 (float d0, float d1, float d2, float d3, svuint16x4_t x) {}
void fn_u32x4 (float d0, float d1, float d2, float d3, svuint32x4_t x) {}
void fn_u64x4 (float d0, float d1, float d2, float d3, svuint64x4_t x) {}
void fn_bf16x4 (float d0, float d1, float d2, float d3, svbfloat16x4_t x) {}
void fn_f16x4 (float d0, float d1, float d2, float d3, svfloat16x4_t x) {}
void fn_f32x4 (float d0, float d1, float d2, float d3, svfloat32x4_t x) {}
void fn_f64x4 (float d0, float d1, float d2, float d3, svfloat64x4_t x) {}

/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s8\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s32\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s64\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u8\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u32\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u64\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_bf16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f32\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f64\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s8x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s32x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s64x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u8x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u32x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u64x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_bf16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f32x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f64x2\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s8x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s32x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s64x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u8x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u32x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u64x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_bf16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f32x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f64x3\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s8x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s32x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_s64x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u8x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u32x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_u64x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_bf16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f32x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tfn_f64x4\n} } } */
