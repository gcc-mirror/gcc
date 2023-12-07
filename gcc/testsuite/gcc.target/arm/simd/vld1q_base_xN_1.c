/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

uint8x16x2_t test_vld1q_u8_x2 (uint8_t * a)
{
    return vld1q_u8_x2 (a);
}

uint16x8x2_t test_vld1q_u16_x2 (uint16_t * a)
{
    return vld1q_u16_x2 (a);
}

uint32x4x2_t test_vld1q_u32_x2 (uint32_t * a)
{
    return vld1q_u32_x2 (a);
}

uint64x2x2_t test_vld1q_u64_x2 (uint64_t * a)
{
    return vld1q_u64_x2 (a);
}

int8x16x2_t test_vld1q_s8_x2 (int8_t * a)
{
    return vld1q_s8_x2 (a);
}

int16x8x2_t test_vld1q_s16_x2 (int16_t * a)
{
    return vld1q_s16_x2 (a);
}

int32x4x2_t test_vld1q_s32_x2 (int32_t * a)
{
    return vld1q_s32_x2 (a);
}

int64x2x2_t test_vld1q_s64_x2 (int64_t * a)
{
    return vld1q_s64_x2 (a);
}

float32x4x2_t test_vld1q_f32_x2 (float32_t * a)
{
    return vld1q_f32_x2 (a);
}

poly8x16x2_t test_vld1q_p8_x2 (poly8_t * a)
{
    return vld1q_p8_x2 (a);
}

poly16x8x2_t test_vld1q_p16_x2 (poly16_t * a)
{
    return vld1q_p16_x2 (a);
}

/* { dg-final { scan-assembler-times {vld1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 2 } }  */

