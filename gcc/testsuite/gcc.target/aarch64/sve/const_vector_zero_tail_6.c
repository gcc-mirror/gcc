/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 1 explicitly initialized
   8-bit integer element is not loaded with a single Advanced SIMD byte load
   or halfword GPR-to-SIMD move because it would not be profitable.  (There is
   no fmov bN.)  */

#include <arm_sve.h>

#define LIT _Literal (uint8_t)

svuint8_t
__GIMPLE (startwith ("expand")) f (void)
{
  svuint8_t v;

  v = _Literal (svuint8_t){LIT 1};

  return v;
}

/* { dg-final { scan-assembler-not {(\tldr\tb[0-9]+,|\tfmov\th[0-9]+, w[0-9]+)} } } */

/* It is more efficient to use a zeroing predicated move.  */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.b, vl1} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, p[0-9]+/z,} 1 } } */
