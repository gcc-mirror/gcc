/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 1 explicitly initialized
   8-bit integer element in a 64-bit container is not loaded with a single
   Advanced SIMD byte load or GPR-to-SIMD halfword move because it would not be
   profitable.  (There is no fmov bN.)  */

#include <arm_sve.h>

typedef uint8_t vnx2qi __attribute__ ((mode (VNx2QI))); /* { dg-warning "specifying vector types.*deprecated" } */
/* { dg-message "use .*vector_size.* instead" "" { target *-*-* } .-1 } */

#define LIT _Literal (uint8_t)

vnx2qi
__GIMPLE (startwith ("expand")) f (void)
{
  vnx2qi v;

  v = _Literal (vnx2qi){LIT 1};

  return v;
}

/* { dg-final { scan-assembler-not {(\tldr\tb[0-9]+,|\tfmov\th[0-9]+, w[0-9]+)} } } */

/* It is more efficient to use a zeroing predicated move.  */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-9]+\.b, vl1} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, p[0-9]+/z,} 1 } } */
