/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 4 explicitly initialized
   8-bit integer elements in 16-bit containers can be loaded with a
   single Advanced SIMD doubleword load or GPR-to-SIMD move.  The remaining
   elements are zero, including those beyond the minimum SVE vector length.  */

#include <arm_sve.h>

typedef uint8_t vnx8qi __attribute__ ((mode (VNx8QI))); /* { dg-warning "specifying vector types.*deprecated" } */
/* { dg-message "use .*vector_size.* instead" "" { target *-*-* } .-1 } */

#define LIT _Literal (uint8_t)

vnx8qi
__GIMPLE (startwith ("expand")) f (void)
{
  vnx8qi v;

  v = _Literal (vnx8qi){LIT 1, LIT 2, LIT 1, LIT 2};

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\td[0-9]+,|\tfmov\td[0-9]+, x[0-9]+)} 1 } } */

/* It is less efficient to broadcast a short repeated vector and then select
   only the low elements.  */
/* { dg-final { scan-assembler-not {\tld1rw\t} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
