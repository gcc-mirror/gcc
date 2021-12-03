/* { dg-do compile { target { aarch64*-*-* } } } */

#include <arm_neon.h>

uint8x16_t foo (uint16x8_t a, uint16x8_t b)
{
  return vrshrn_high_n_u16 (vrshrn_n_u16 (a, 8), b, 8);
}

/* { dg-final { scan-assembler-times {\traddhn\t} 1 } } */
/* { dg-final { scan-assembler-times {\traddhn2\t} 1 } } */
