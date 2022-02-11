/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps --param=vect-epilogues-nomask=0" } */


#include <arm_neon.h>

uint32x4_t foo (uint64x2_t a, uint64x2_t b)
{
  return vshrn_high_n_u64 (vshrn_n_u64 (a, 32), b, 32);
}

/* { dg-final { scan-assembler-times {\tuzp2\t} 1 } } */
/* { dg-final { scan-assembler-not {\tshrn\t} } } */
/* { dg-final { scan-assembler-not {\tshrn2\t} } } */
