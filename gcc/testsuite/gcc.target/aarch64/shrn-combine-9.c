/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */


#include <arm_neon.h>

uint16x8_t foo (uint32x4_t a, uint32x4_t b)
{
  return vshrn_high_n_u32 (vshrn_n_u32 (a, 16), b, 16);
}

/* { dg-final { scan-assembler-times {\tuzp2\t} 1 } } */
/* { dg-final { scan-assembler-not {\tshrn\t} } } */
/* { dg-final { scan-assembler-not {\tshrn2\t} } } */
