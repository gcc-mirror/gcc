/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 8 explicitly initialized
   8-bit integer elements can be loaded with a single Advanced SIMD doubleword
   load or GPR-to-SIMD move. The remaining elements are zero, including those
   beyond the minimum SVE vector length.  */

#include <arm_sve.h>

#define LIT _Literal (uint8_t)

svuint8_t
__GIMPLE (startwith ("expand")) f (void)
{
  svuint8_t v;

  v = _Literal (svuint8_t){
    LIT 1, LIT 2, LIT 1, LIT 2,
    LIT 1, LIT 2, LIT 1, LIT 2
  };

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\td[0-9]+,|\tfmov\td[0-9]+, x[0-9]+)} 1 } } */

/* The general-purpose expansion instead broadcasts a short repeated vector and
   then selects only the low elements.  */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.h, w[0-9]+} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
