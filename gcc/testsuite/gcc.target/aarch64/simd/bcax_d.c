/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#pragma GCC target "+sha3"

#define BCAX(x,y,z)  ((x) ^ ((y) & ~(z)))

/* When the inputs come from GP regs don't form a BCAX.  */
uint64_t bcax_d_gp (uint64_t a, uint64_t b, uint64_t c) { return BCAX (a, b, c); }

uint64x1_t bcax_d (uint64x1_t a, uint64x1_t b, uint64x1_t c) { return BCAX (a, b, c); }
uint32x2_t bcax_s (uint32x2_t a, uint32x2_t b, uint32x2_t c) { return BCAX (a, b, c); }
uint16x4_t bcax_h (uint16x4_t a, uint16x4_t b, uint16x4_t c) { return BCAX (a, b, c); }
uint8x8_t bcax_b (uint8x8_t a, uint8x8_t b, uint8x8_t c) { return BCAX (a, b, c); }

/* { dg-final { scan-assembler-times {bcax\tv0.16b, v0.16b, v1.16b, v2.16b} 4 } } */

