/* { dg-options "-g" } */

#include <arm_sve.h>

svbool_t f_b (svbool_t x) { return svptrue_b32 (); }
svint8_t f_s8 (svint8_t x) { return svdup_s8 (0); }
svuint8_t f_u8 (svuint8_t x) { return svdup_u8 (1); }
svint16_t f_s16 (svint16_t x) { return svdup_s16 (2); }
svuint16_t f_u16 (svuint16_t x) { return svdup_u16 (3); }
svfloat16_t f_f16 (svfloat16_t x) { return svdup_f16 (4); }
svint32_t f_s32 (svint32_t x) { return svdup_s32 (5); }
svuint32_t f_u32 (svuint32_t x) { return svdup_u32 (6); }
svfloat32_t f_f32 (svfloat32_t x) { return svdup_f32 (7); }
svint64_t f_s64 (svint64_t x) { return svdup_s64 (8); }
svuint64_t f_u64 (svuint64_t x) { return svdup_u64 (9); }
svfloat64_t f_f64 (svfloat64_t x) { return svdup_f64 (10); }
