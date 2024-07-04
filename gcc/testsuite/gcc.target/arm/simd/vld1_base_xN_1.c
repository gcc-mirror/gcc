/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

uint8x8x2_t test_vld1_u8_x2 (uint8_t * a)
{
    return vld1_u8_x2 (a);
}

uint16x4x2_t test_vld1_u16_x2 (uint16_t * a)
{
    return vld1_u16_x2 (a);
}

uint32x2x2_t test_vld1_u32_x2 (uint32_t * a)
{
    return vld1_u32_x2 (a);
}

uint64x1x2_t test_vld1_u64_x2 (uint64_t * a)
{
    return vld1_u64_x2 (a);
}

int8x8x2_t test_vld1_s8_x2 (int8_t * a)
{
    return vld1_s8_x2 (a);
}

int16x4x2_t test_vld1_s16_x2 (int16_t * a)
{
    return vld1_s16_x2 (a);
}

int32x2x2_t test_vld1_s32_x2 (int32_t * a)
{
    return vld1_s32_x2 (a);
}

int64x1x2_t test_vld1_s64_x2 (int64_t * a)
{
    return vld1_s64_x2 (a);
}

float32x2x2_t test_vld1_f32_x2 (float32_t * a)
{
    return vld1_f32_x2 (a);
}

poly8x8x2_t test_vld1_p8_x2 (poly8_t * a)
{
    return vld1_p8_x2 (a);
}

poly16x4x2_t test_vld1_p16_x2 (poly16_t * a)
{
    return vld1_p16_x2 (a);
}

uint8x8x3_t test_vld1_u8_x3 (uint8_t * a)
{
    return vld1_u8_x3 (a);
}

uint16x4x3_t test_vld1_u16_x3 (uint16_t * a)
{
    return vld1_u16_x3 (a);
}

uint32x2x3_t test_vld1_u32_x3 (uint32_t * a)
{
    return vld1_u32_x3 (a);
}

uint64x1x3_t test_vld1_u64_x3 (uint64_t * a)
{
    return vld1_u64_x3 (a);
}

int8x8x3_t test_vld1_s8_x3 (int8_t * a)
{
    return vld1_s8_x3 (a);
}

int16x4x3_t test_vld1_s16_x3 (int16_t * a)
{
    return vld1_s16_x3 (a);
}

int32x2x3_t test_vld1_s32_x3 (int32_t * a)
{
    return vld1_s32_x3 (a);
}

int64x1x3_t test_vld1_s64_x3 (int64_t * a)
{
    return vld1_s64_x3 (a);
}

float32x2x3_t test_vld1_f32_x3 (float32_t * a)
{
    return vld1_f32_x3 (a);
}

poly8x8x3_t test_vld1_p8_x3 (poly8_t * a)
{
    return vld1_p8_x3 (a);
}

poly16x4x3_t test_vld1_p16_x3 (poly16_t * a)
{
    return vld1_p16_x3 (a);
}

uint8x8x4_t test_vld1_u8_x4 (uint8_t * a)
{
    return vld1_u8_x4 (a);
}

uint16x4x4_t test_vld1_u16_x4 (uint16_t * a)
{
    return vld1_u16_x4 (a);
}

uint32x2x4_t test_vld1_u32_x4 (uint32_t * a)
{
    return vld1_u32_x4 (a);
}

uint64x1x4_t test_vld1_u64_x4 (uint64_t * a)
{
    return vld1_u64_x4 (a);
}

int8x8x4_t test_vld1_s8_x4 (int8_t * a)
{
    return vld1_s8_x4 (a);
}

int16x4x4_t test_vld1_s16_x4 (int16_t * a)
{
    return vld1_s16_x4 (a);
}

int32x2x4_t test_vld1_s32_x4 (int32_t * a)
{
    return vld1_s32_x4 (a);
}

int64x1x4_t test_vld1_s64_x4 (int64_t * a)
{
    return vld1_s64_x4 (a);
}

float32x2x4_t test_vld1_f32_x4 (float32_t * a)
{
    return vld1_f32_x4 (a);
}

poly8x8x4_t test_vld1_p8_x4 (poly8_t * a)
{
    return vld1_p8_x4 (a);
}

poly16x4x4_t test_vld1_p16_x4 (poly16_t * a)
{
    return vld1_p16_x4 (a);
}

/* { dg-final { scan-assembler-times {vld1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vld1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vld1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 6 } }  */