/* { dg-do compile } */

#include <arm_sve.h>

svbool_t ret_b (void) { return svptrue_b8 (); }

svcount_t ret_c (svcount_t *ptr) { return *ptr; }

svint8_t ret_s8 (void) { return svdup_s8 (0); }
svint16_t ret_s16 (void) { return svdup_s16 (0); }
svint32_t ret_s32 (void) { return svdup_s32 (0); }
svint64_t ret_s64 (void) { return svdup_s64 (0); }
svuint8_t ret_u8 (void) { return svdup_u8 (0); }
svuint16_t ret_u16 (void) { return svdup_u16 (0); }
svuint32_t ret_u32 (void) { return svdup_u32 (0); }
svuint64_t ret_u64 (void) { return svdup_u64 (0); }
svmfloat8_t ret_mf8 (void) { return svundef_mf8 (); }
svbfloat16_t ret_bf16 (void) { return svundef_bf16 (); }
svfloat16_t ret_f16 (void) { return svdup_f16 (0); }
svfloat32_t ret_f32 (void) { return svdup_f32 (0); }
svfloat64_t ret_f64 (void) { return svdup_f64 (0); }

svint8x2_t ret_s8x2 (void) { return svundef2_s8 (); }
svint16x2_t ret_s16x2 (void) { return svundef2_s16 (); }
svint32x2_t ret_s32x2 (void) { return svundef2_s32 (); }
svint64x2_t ret_s64x2 (void) { return svundef2_s64 (); }
svuint8x2_t ret_u8x2 (void) { return svundef2_u8 (); }
svuint16x2_t ret_u16x2 (void) { return svundef2_u16 (); }
svuint32x2_t ret_u32x2 (void) { return svundef2_u32 (); }
svuint64x2_t ret_u64x2 (void) { return svundef2_u64 (); }
svmfloat8x2_t ret_mf8x2 (void) { return svundef2_mf8 (); }
svbfloat16x2_t ret_bf16x2 (void) { return svundef2_bf16 (); }
svfloat16x2_t ret_f16x2 (void) { return svundef2_f16 (); }
svfloat32x2_t ret_f32x2 (void) { return svundef2_f32 (); }
svfloat64x2_t ret_f64x2 (void) { return svundef2_f64 (); }

svint8x3_t ret_s8x3 (void) { return svundef3_s8 (); }
svint16x3_t ret_s16x3 (void) { return svundef3_s16 (); }
svint32x3_t ret_s32x3 (void) { return svundef3_s32 (); }
svint64x3_t ret_s64x3 (void) { return svundef3_s64 (); }
svuint8x3_t ret_u8x3 (void) { return svundef3_u8 (); }
svuint16x3_t ret_u16x3 (void) { return svundef3_u16 (); }
svuint32x3_t ret_u32x3 (void) { return svundef3_u32 (); }
svuint64x3_t ret_u64x3 (void) { return svundef3_u64 (); }
svmfloat8x3_t ret_mf8x3 (void) { return svundef3_mf8 (); }
svbfloat16x3_t ret_bf16x3 (void) { return svundef3_bf16 (); }
svfloat16x3_t ret_f16x3 (void) { return svundef3_f16 (); }
svfloat32x3_t ret_f32x3 (void) { return svundef3_f32 (); }
svfloat64x3_t ret_f64x3 (void) { return svundef3_f64 (); }

svint8x4_t ret_s8x4 (void) { return svundef4_s8 (); }
svint16x4_t ret_s16x4 (void) { return svundef4_s16 (); }
svint32x4_t ret_s32x4 (void) { return svundef4_s32 (); }
svint64x4_t ret_s64x4 (void) { return svundef4_s64 (); }
svuint8x4_t ret_u8x4 (void) { return svundef4_u8 (); }
svuint16x4_t ret_u16x4 (void) { return svundef4_u16 (); }
svuint32x4_t ret_u32x4 (void) { return svundef4_u32 (); }
svuint64x4_t ret_u64x4 (void) { return svundef4_u64 (); }
svmfloat8x4_t ret_mf8x4 (void) { return svundef4_mf8 (); }
svbfloat16x4_t ret_bf16x4 (void) { return svundef4_bf16 (); }
svfloat16x4_t ret_f16x4 (void) { return svundef4_f16 (); }
svfloat32x4_t ret_f32x4 (void) { return svundef4_f32 (); }
svfloat64x4_t ret_f64x4 (void) { return svundef4_f64 (); }

/* { dg-final { scan-assembler {\t\.variant_pcs\tret_b\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tret_c\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s8\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s32\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s64\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u8\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u32\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u64\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_mf8\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_bf16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f16\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f32\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f64\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s8x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s32x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s64x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u8x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u32x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u64x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_mf8x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_bf16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f16x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f32x2\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f64x2\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s8x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s32x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s64x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u8x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u32x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u64x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_mf8x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_bf16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f16x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f32x3\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f64x3\n} } } */

/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s8x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s32x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_s64x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u8x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u32x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_u64x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_mf8x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_bf16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f16x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f32x4\n} } } */
/* { dg-final { scan-assembler {\t\.variant_pcs\tret_f64x4\n} } } */
