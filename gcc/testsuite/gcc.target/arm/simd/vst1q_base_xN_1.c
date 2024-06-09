/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"


void test_vst1q_u8_x2 (uint8_t * ptr, uint8x16x2_t val)
{
    vst1q_u8_x2 (ptr, val);
}

void test_vst1q_u16_x2 (uint16_t * ptr, uint16x8x2_t val)
{
    vst1q_u16_x2 (ptr, val);
}

void test_vst1q_u32_x2 (uint32_t * ptr, uint32x4x2_t val)
{
    vst1q_u32_x2 (ptr, val);
}

void test_vst1q_u64_x2 (uint64_t * ptr, uint64x2x2_t val)
{
    vst1q_u64_x2 (ptr, val);
}

void test_vst1q_s8_x2 (int8_t * ptr, int8x16x2_t val)
{
    vst1q_s8_x2 (ptr, val);
}

void test_vst1q_s16_x2 (int16_t * ptr, int16x8x2_t val)
{
    vst1q_s16_x2 (ptr, val);
}

void test_vst1q_s32_x2 (int32_t * ptr, int32x4x2_t val)
{
    vst1q_s32_x2 (ptr, val);
}

void test_vst1q_s64_x2 (int64_t * ptr, int64x2x2_t val)
{
    vst1q_s64_x2 (ptr, val);
}

void test_vst1q_f32_x2 (float32_t * ptr, float32x4x2_t val)
{
    vst1q_f32_x2 (ptr, val);
}

void test_vst1q_p8_x2 (poly8_t * ptr, poly8x16x2_t val)
{
    vst1q_p8_x2 (ptr, val);
}

void test_vst1q_p16_x2 (poly16_t * ptr, poly16x8x2_t val)
{
    vst1q_p16_x2 (ptr, val);
}

void test_vst1q_u8_x3 (uint8_t * ptr, uint8x16x3_t val)
{
    vst1q_u8_x3 (ptr, val);
}

void test_vst1q_u16_x3 (uint16_t * ptr, uint16x8x3_t val)
{
    vst1q_u16_x3 (ptr, val);
}

void test_vst1q_u32_x3 (uint32_t * ptr, uint32x4x3_t val)
{
    vst1q_u32_x3 (ptr, val);
}

void test_vst1q_u64_x3 (uint64_t * ptr, uint64x2x3_t val)
{
    vst1q_u64_x3 (ptr, val);
}

void test_vst1q_s8_x3 (int8_t * ptr, int8x16x3_t val)
{
    vst1q_s8_x3 (ptr, val);
}

void test_vst1q_s16_x3 (int16_t * ptr, int16x8x3_t val)
{
    vst1q_s16_x3 (ptr, val);
}

void test_vst1q_s32_x3 (int32_t * ptr, int32x4x3_t val)
{
    vst1q_s32_x3 (ptr, val);
}

void test_vst1q_s64_x3 (int64_t * ptr, int64x2x3_t val)
{
    vst1q_s64_x3 (ptr, val);
}

void test_vst1q_f32_x3 (float32_t * ptr, float32x4x3_t val)
{
    vst1q_f32_x3 (ptr, val);
}

void test_vst1q_p8_x3 (poly8_t * ptr, poly8x16x3_t val)
{
    vst1q_p8_x3 (ptr, val);
}

void test_vst1q_p16_x3 (poly16_t * ptr, poly16x8x3_t val)
{
    vst1q_p16_x3 (ptr, val);
}

void test_vst1q_u8_x4 (uint8_t * ptr, uint8x16x4_t val)
{
    vst1q_u8_x4 (ptr, val);
}

void test_vst1q_u16_x4 (uint16_t * ptr, uint16x8x4_t val)
{
    vst1q_u16_x4 (ptr, val);
}

void test_vst1q_u32_x4 (uint32_t * ptr, uint32x4x4_t val)
{
    vst1q_u32_x4 (ptr, val);
}

void test_vst1q_u64_x4 (uint64_t * ptr, uint64x2x4_t val)
{
    vst1q_u64_x4 (ptr, val);
}

void test_vst1q_s8_x4 (int8_t * ptr, int8x16x4_t val)
{
    vst1q_s8_x4 (ptr, val);
}

void test_vst1q_s16_x4 (int16_t * ptr, int16x8x4_t val)
{
    vst1q_s16_x4 (ptr, val);
}

void test_vst1q_s32_x4 (int32_t * ptr, int32x4x4_t val)
{
    vst1q_s32_x4 (ptr, val);
}

void test_vst1q_s64_x4 (int64_t * ptr, int64x2x4_t val)
{
    vst1q_s64_x4 (ptr, val);
}

void test_vst1q_f32_x4 (float32_t * ptr, float32x4x4_t val)
{
    vst1q_f32_x4 (ptr, val);
}

void test_vst1q_p8_x4 (poly8_t * ptr, poly8x16x4_t val)
{
    vst1q_p8_x4 (ptr, val);
}

void test_vst1q_p16_x4 (poly16_t * ptr, poly16x8x4_t val)
{
    vst1q_p16_x4 (ptr, val);
}


/* { dg-final { scan-assembler-times {vst1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vst1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 6 } }  */

/* { dg-final { scan-assembler-times {vst1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vst1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 6 } }  */

/* { dg-final { scan-assembler-times {vst1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vst1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 6 } }  */

/* { dg-final { scan-assembler-times {vst1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 6 } }  */
/* { dg-final { scan-assembler-times {vst1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]!\n} 4 } }  */
