/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_neon_fp16 } */

#include "arm_neon.h"

float16x4x2_t test_vld1_f16_x2 (float16_t * a)
{
    return vld1_f16_x2 (a);
}

float16x4x3_t test_vld1_f16_x3 (float16_t * a)
{
    return vld1_f16_x3 (a);
}

float16x4x4_t test_vld1_f16_x4 (float16_t * a)
{
    return vld1_f16_x4 (a);
}

/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
