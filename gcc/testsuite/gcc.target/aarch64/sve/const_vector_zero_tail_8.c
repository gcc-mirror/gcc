/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 1 explicitly initialized
   64-bit floating-point element can be loaded with a single Advanced SIMD
   doubleword load or GPR-to-SIMD move.  The remaining elements are zero,
   including those beyond the minimum SVE vector length.  */

#include <arm_sve.h>

#define LIT _Literal (float64_t)

svfloat64_t
__GIMPLE (startwith ("expand")) f (void)
{
  svfloat64_t v;

  v = _Literal (svfloat64_t){LIT 1.234375f};

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\td[0-9]+,|\tfmov\td[0-9]+, x[0-9]+)} 1 } } */

/* It is less efficient to broadcast a short repeated vector and then select
   only the low elements.  */
/* { dg-final { scan-assembler-not {\tld1rd\t} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
