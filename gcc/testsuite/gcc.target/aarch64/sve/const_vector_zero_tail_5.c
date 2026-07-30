/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 1 explicitly initialized
   64-bit integer element can be loaded with a single Advanced SIMD doubleword
   load or GPR-to-SIMD move.  The remaining elements are zero, including those
   beyond the minimum SVE vector length.  */

#include <arm_sve.h>

#define LIT _Literal (uint64_t)

svuint64_t
__GIMPLE (startwith ("expand")) f (void)
{
  svuint64_t v;

  v = _Literal (svuint64_t){LIT 0x123456789abcdef0ULL};

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\td[0-9]+,|\tfmov\td[0-9]+, x[0-9]+)} 1 } } */

/* The general-purpose expansion instead broadcasts a short repeated vector and
   then selects only the low elements.  */
/* { dg-final { scan-assembler-not {\tld1rd\t} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
