/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sm4" } */

#include <arm_neon.h>

uint32x4_t bar (uint32x4_t a, uint32x4_t b, uint32x4_t c) {
        return vsm3tt1aq_u32(a, b, c, 2);
}

/* { dg-final { scan-assembler {\tsm3tt1a\t} } } */
