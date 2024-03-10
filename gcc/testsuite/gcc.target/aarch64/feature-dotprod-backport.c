/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+dotprod" } */

#include <arm_neon.h>

uint32x4_t bar (uint32x4_t r, uint8x16_t a, uint8x16_t b) {
        return vdotq_u32(r, a, b);
}

/* { dg-final { scan-assembler {\tudot\t} } } */
