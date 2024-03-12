/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+fp16" } */

#include <arm_neon.h>

float16x8_t bar (float16x8_t a, float16x8_t b) {
        return vaddq_f16(a, b);
}

/* { dg-final { scan-assembler {\tfadd\t} } } */
