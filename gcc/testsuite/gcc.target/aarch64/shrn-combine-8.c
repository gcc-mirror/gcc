/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */


#include <arm_neon.h>

uint8x16_t foo (uint16x8_t a, uint16x8_t b)
{
  return vshrn_high_n_u16 (vshrn_n_u16 (a, 8), b, 8);
}

/* { dg-final { scan-assembler-times {\tuzp2\t} 1 } } */
/* { dg-final { scan-assembler-not {\tshrn\t} } } */
/* { dg-final { scan-assembler-not {\tshrn2\t} } } */
