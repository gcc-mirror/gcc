/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+fp16+nosimd" } */

#include <arm_fp16.h>

float16_t bar (float16_t a, float16_t b) {
        return vaddh_f16(a, b);
}

/* { dg-final { scan-assembler {\tfadd\t} } } */
