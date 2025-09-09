/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#pragma GCC target "+sha3"

#define BCAX(x,y,z)  ((x) ^ ((y) & ~(z)))

uint32x2_t bcax_s (uint32x2_t a, uint32x2_t b, uint32x2_t c) { return BCAX (a, b, c); }
uint16x4_t bcax_h (uint16x4_t a, uint16x4_t b, uint16x4_t c) { return BCAX (a, b, c); }
uint8x8_t bcax_b (uint8x8_t a, uint8x8_t b, uint8x8_t c) { return BCAX (a, b, c); }

/* { dg-final { scan-assembler-times {bcax\tv0.16b, v0.16b, v1.16b, v2.16b} 3 } } */

