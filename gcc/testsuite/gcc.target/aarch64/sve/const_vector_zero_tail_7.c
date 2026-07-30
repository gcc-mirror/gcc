/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 2 explicitly initialized
   8-bit integer elements can be loaded with a single Advanced SIMD halfword
   load or GPR-to-SIMD move.  The remaining elements are zero, including those
   beyond the minimum SVE vector length.  */

#include <arm_sve.h>

#define LIT _Literal (uint8_t)

svuint8_t
__GIMPLE (startwith ("expand")) f (void)
{
  svuint8_t v;

  v = _Literal (svuint8_t){LIT 1, LIT 2};

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\th[0-9]+,|\tfmov\th[0-9]+, w[0-9]+)} 1 } } */

/* It is less efficient to broadcast a short repeated vector and then select
   only the low elements.  */
/* { dg-final { scan-assembler-not {\tmov\tz[0-9]+\.h,} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
