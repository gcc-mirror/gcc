/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#pragma GCC target "+sha3"

#define EOR3(x,y,z)  ((x) ^ (y) ^ (z))

uint32x2_t bcax_s (uint32x2_t a, uint32x2_t b, uint32x2_t c) { return EOR3 (a, b, c); }
uint16x4_t bcax_h (uint16x4_t a, uint16x4_t b, uint16x4_t c) { return EOR3 (a, b, c); }
uint8x8_t bcax_b (uint8x8_t a, uint8x8_t b, uint8x8_t c) { return EOR3 (a, b, c); }

/* { dg-final { scan-assembler-times {eor3\tv0.16b, v[0-9]+.16b, v[0-9]+.16b, v[0-9]+.16b} 3 } } */

