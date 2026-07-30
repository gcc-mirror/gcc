/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 3 explicitly initialized
   16-bit floating-point elements can be loaded with a single Advanced SIMD
   doubleword load or GPR-to-SIMD move.  The remaining elements are zero,
   including those beyond the minimum SVE vector length.  */

#include <arm_sve.h>

#define LIT _Literal (float16_t)

svfloat16_t
__GIMPLE (startwith ("expand")) f (void)
{
  svfloat16_t v;

  v = _Literal (svfloat16_t){
    LIT 1.0f, LIT 2.0f, LIT 3.0f
  };

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\td[0-9]+,|\tfmov\td[0-9]+, x[0-9]+)} 1 } } */

/* The general-purpose expansion instead broadcasts a short repeated vector and
   then selects only the low elements.  */
/* { dg-final { scan-assembler-not {\tld1rd\t} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
