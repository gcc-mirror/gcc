/* { dg-options "-g" } */

#include <arm_sve.h>

svbool_t f_b (svbool_t x) { return x; }
svint8_t f_s8 (svint8_t x) { return x; }
svuint8_t f_u8 (svuint8_t x) { return x; }
svint16_t f_s16 (svint16_t x) { return x; }
svuint16_t f_u16 (svuint16_t x) { return x; }
svfloat16_t f_f16 (svfloat16_t x) { return x; }
svint32_t f_s32 (svint32_t x) { return x; }
svuint32_t f_u32 (svuint32_t x) { return x; }
svfloat32_t f_f32 (svfloat32_t x) { return x; }
svint64_t f_s64 (svint64_t x) { return x; }
svuint64_t f_u64 (svuint64_t x) { return x; }
svfloat64_t f_f64 (svfloat64_t x) { return x; }
