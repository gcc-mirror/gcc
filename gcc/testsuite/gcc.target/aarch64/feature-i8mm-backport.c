/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+i8mm" } */

#include <arm_neon.h>

int32x4_t bar (int32x4_t r, int8x16_t a, int8x16_t b) {
        return vmmlaq_s32 (r, a, b);
}

/* { dg-final { scan-assembler {\tsmmla\t} } } */
