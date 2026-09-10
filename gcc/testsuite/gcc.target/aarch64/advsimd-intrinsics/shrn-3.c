/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

#include <arm_neon.h>

uint16x8_t foo (uint32x4_t a, uint32x4_t b)
{
  return vrshrn_high_n_u32 (vrshrn_n_u32 (a, 16), b, 16);
}

/* { dg-final { scan-assembler-times {\trshrn\t} 1 } } */
/* { dg-final { scan-assembler-times {\trshrn2\t} 1 } } */
