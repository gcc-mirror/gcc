/* { dg-do compile { target { aarch64*-*-* } } } */

#include <arm_neon.h>

uint32x4_t foo (uint64x2_t a, uint64x2_t b)
{
  return vrshrn_high_n_u64 (vrshrn_n_u64 (a, 32), b, 32);
}

/* { dg-final { scan-assembler-times {\traddhn\t} 1 } } */
/* { dg-final { scan-assembler-times {\traddhn2\t} 1 } } */
