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

/* { dg-final { scan-assembler-times {vld1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 2 } }  */