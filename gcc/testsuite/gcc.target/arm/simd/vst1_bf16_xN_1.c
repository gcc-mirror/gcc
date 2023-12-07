/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_v8_2a_bf16_neon } */

#include "arm_neon.h"

void test_vst1_bf16_x2 (bfloat16_t * ptr, bfloat16x4x2_t val)
{
    vst1_bf16_x2 (ptr, val);
}

void test_vst1_bf16_x3 (bfloat16_t * ptr, bfloat16x4x3_t val)
{
    vst1_bf16_x3 (ptr, val);
}

void test_vst1_bf16_x4 (bfloat16_t * ptr, bfloat16x4x4_t val)
{
    vst1_bf16_x4 (ptr, val);
}
/* { dg-final { scan-assembler-times {vst1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
