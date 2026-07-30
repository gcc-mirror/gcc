/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 2 explicitly initialized
   16-bit integer elements in 32-bit containers can be loaded with a
   single Advanced SIMD doubleword load or GPR-to-SIMD move.  The remaining
   elements are zero, including those beyond the minimum SVE vector length.  */

#include <arm_sve.h>

typedef uint16_t vnx4hi __attribute__ ((mode (VNx4HI))); /* { dg-warning "specifying vector types.*deprecated" } */
/* { dg-message "use .*vector_size.* instead" "" { target *-*-* } .-1 } */

#define LIT _Literal (uint16_t)

vnx4hi
__GIMPLE (startwith ("expand")) f (void)
{
  vnx4hi v;

  v = _Literal (vnx4hi){LIT 1, LIT 2};

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\td[0-9]+,|\tfmov\td[0-9]+, x[0-9]+)} 1 } } */

/* It is less efficient to broadcast a short repeated vector and then select
   only the low elements.  */
/* { dg-final { scan-assembler-not {\tld1rd\t} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
