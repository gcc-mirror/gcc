/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

#include <arm_neon.h>

uint8x16_t foo (uint32x4_t a, uint32x4_t b)
{
  uint16x4_t a1 = vrshrn_n_u32 (a, 16);
  uint16x8_t b1 = vrshrn_high_n_u32 (a1, b, 16);
  return vrshrn_high_n_u16 (vrshrn_n_u16 (b1, 8), b1, 8);
}

/* { dg-final { scan-assembler-times {\tmovi\t} 1 } } */
/* { dg-final { scan-assembler-times {\traddhn\t} 2 } } */
/* { dg-final { scan-assembler-times {\traddhn2\t} 2 } } */
