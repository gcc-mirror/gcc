/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-save-temps -O2 -Wpointer-sign" } */
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

uint8x16x3_t test_vld1q_u8_x3 (uint8_t * a)
{
    return vld1q_u8_x3 (a);
}

uint16x8x3_t test_vld1q_u16_x3 (uint16_t * a)
{
    return vld1q_u16_x3 (a);
}

uint32x4x3_t test_vld1q_u32_x3 (uint32_t * a)
{
    return vld1q_u32_x3 (a);
}

uint64x2x3_t test_vld1q_u64_x3 (uint64_t * a)
{
    return vld1q_u64_x3 (a);
}

int8x16x3_t test_vld1q_s8_x3 (int8_t * a)
{
    return vld1q_s8_x3 (a);
}

int16x8x3_t test_vld1q_s16_x3 (int16_t * a)
{
    return vld1q_s16_x3 (a);
}

int32x4x3_t test_vld1q_s32_x3 (int32_t * a)
{
    return vld1q_s32_x3 (a);
}

int64x2x3_t test_vld1q_s64_x3 (int64_t * a)
{
    return vld1q_s64_x3 (a);
}

float32x4x3_t test_vld1q_f32_x3 (float32_t * a)
{
    return vld1q_f32_x3 (a);
}

poly8x16x3_t test_vld1q_p8_x3 (poly8_t * a)
{
    return vld1q_p8_x3 (a);
}

poly16x8x3_t test_vld1q_p16_x3 (poly16_t * a)
{
    return vld1q_p16_x3 (a);
}

uint8x16x4_t test_vld1q_u8_x4 (uint8_t * a)
{
    return vld1q_u8_x4 (a);
}

uint16x8x4_t test_vld1q_u16_x4 (uint16_t * a)
{
    return vld1q_u16_x4 (a);
}

uint32x4x4_t test_vld1q_u32_x4 (uint32_t * a)
{
    return vld1q_u32_x4 (a);
}

uint64x2x4_t test_vld1q_u64_x4 (uint64_t * a)
{
    return vld1q_u64_x4 (a);
}

int8x16x4_t test_vld1q_s8_x4 (int8_t * a)
{
    return vld1q_s8_x4 (a);
}

int16x8x4_t test_vld1q_s16_x4 (int16_t * a)
{
    return vld1q_s16_x4 (a);
}

int32x4x4_t test_vld1q_s32_x4 (int32_t * a)
{
    return vld1q_s32_x4 (a);
}

int64x2x4_t test_vld1q_s64_x4 (int64_t * a)
{
    return vld1q_s64_x4 (a);
}

float32x4x4_t test_vld1q_f32_x4 (float32_t * a)
{
    return vld1q_f32_x4 (a);
}

poly8x16x4_t test_vld1q_p8_x4 (poly8_t * a)
{
    return vld1q_p8_x4 (a);
}

poly16x8x4_t test_vld1q_p16_x4 (poly16_t * a)
{
    return vld1q_p16_x4 (a);
}

/* { dg-final { scan-assembler-times {vld1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vld1.8\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 6 } }  */

/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 6 } }  */

/* { dg-final { scan-assembler-times {vld1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 9 } }  */
/* { dg-final { scan-assembler-times {vld1.32\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 6 } }  */

/* { dg-final { scan-assembler-times {vld1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]\n} 6 } }  */
/* { dg-final { scan-assembler-times {vld1.64\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+:64\]!\n} 4 } }  */
