/* { dg-options "-O2" } */

#include <arm_neon.h>

void test_s8 (int8x8x2_t *ptr) { *ptr = (int8x8x2_t) {}; }
void test_u8 (uint8x8x2_t *ptr) { *ptr = (uint8x8x2_t) {}; }
void test_p8 (poly8x8x2_t *ptr) { *ptr = (poly8x8x2_t) {}; }
void test_s16 (int16x4x2_t *ptr) { *ptr = (int16x4x2_t) {}; }
void test_u16 (uint16x4x2_t *ptr) { *ptr = (uint16x4x2_t) {}; }
void test_p16 (poly16x4x2_t *ptr) { *ptr = (poly16x4x2_t) {}; }
void test_bf16 (bfloat16x4x2_t *ptr) { *ptr = (bfloat16x4x2_t) {}; }
void test_f16 (float16x4x2_t *ptr) { *ptr = (float16x4x2_t) {}; }
void test_s32 (int32x2x2_t *ptr) { *ptr = (int32x2x2_t) {}; }
void test_u32 (uint32x2x2_t *ptr) { *ptr = (uint32x2x2_t) {}; }
void test_f32 (float32x2x2_t *ptr) { *ptr = (float32x2x2_t) {}; }
void test_s64 (int64x1x2_t *ptr) { *ptr = (int64x1x2_t) {}; }
void test_u64 (uint64x1x2_t *ptr) { *ptr = (uint64x1x2_t) {}; }
void test_p64 (poly64x1x2_t *ptr) { *ptr = (poly64x1x2_t) {}; }
void test_f64 (float64x1x2_t *ptr) { *ptr = (float64x1x2_t) {}; }

/* { dg-final { scan-assembler-times {\tstp\txzr, xzr, \[x0\]\n} 15 } } */
