/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+bf16" } */

#include <arm_neon.h>

float32x4_t bar (float32x4_t r, bfloat16x8_t a, bfloat16x8_t b) {
        return vbfmlalbq_f32 (r, a, b);
}

/* { dg-final { scan-assembler {\tbfmlalb\t} } } */
