/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

bfloat16x8x2_t test_vld1q_bf16_x2 (bfloat16_t * a)
{
    return vld1q_bf16_x2 (a);
}

bfloat16x8x3_t test_vld1q_bf16_x3 (bfloat16_t * a)
{
    return vld1q_bf16_x3 (a);
}

bfloat16x8x4_t test_vld1q_bf16_x4 (bfloat16_t * a)
{
    return vld1q_bf16_x4 (a);
}

/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vld1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 2 } }  */
