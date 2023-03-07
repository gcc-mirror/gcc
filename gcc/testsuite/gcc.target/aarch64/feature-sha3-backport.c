/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sha3" } */

#include <arm_neon.h>

uint64x2_t bar (uint64x2_t a, uint64x2_t b, uint64x2_t c) {
        return vsha512hq_u64(a, b, c);
}

/* { dg-final { scan-assembler {\tsha512h\t} } } */
