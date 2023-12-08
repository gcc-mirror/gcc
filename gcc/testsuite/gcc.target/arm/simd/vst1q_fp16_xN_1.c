/* { dg-do assemble } */
/* { dg-require-effective-target arm_neon_fp16_ok } */
/* { dg-options "-save-temps -O2" } */
/* { dg-add-options arm_neon_fp16 } */

#include "arm_neon.h"

void test_vst1q_f16_x2 (float16_t * ptr, float16x8x2_t val)
{
    vst1q_f16_x2 (ptr, val);
}

/* { dg-final { scan-assembler-times {vst1.16\t\{d[0-9]+-d[0-9]+\}, \[r[0-9]+\]\n} 1 } }  */
