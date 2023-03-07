/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+fp16fml" } */

#include <arm_neon.h>

float32x4_t bar (float32x4_t r, float16x8_t a, float16x8_t b) {
        return vfmlalq_high_f16 (r, a, b);
}

/* { dg-final { scan-assembler {\tfmlal2\t} } } */
