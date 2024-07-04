/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_neon_fp16 } */

#include "arm_neon.h"

void test_vst1q_f16_x2 (float16_t * ptr, float16x8x2_t val)
{
    vst1q_f16_x2 (ptr, val);
}

void test_vst1q_f16_x3 (float16_t * ptr, float16x8x3_t val)
{
    vst1q_f16_x3 (ptr, val);
}

void test_vst1q_f16_x4 (float16_t * ptr, float16x8x4_t val)
{
    vst1q_f16_x4 (ptr, val);
}

/* { dg-final { scan-assembler-times {vst1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 3 } }  */
/* { dg-final { scan-assembler-times {vst1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]!\n} 2 } }  */
