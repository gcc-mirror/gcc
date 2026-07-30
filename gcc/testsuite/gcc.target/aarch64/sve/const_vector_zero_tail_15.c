/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* Check that a scalable vector constant with 1 explicitly initialized
   32-bit integer element in a 64-bit container can be loaded with a single
   Advanced SIMD word load or GPR-to-SIMD move.  The remaining elements are
   zero, including those beyond the minimum SVE vector length.  */

#include <arm_sve.h>

typedef uint32_t vnx2si __attribute__ ((mode (VNx2SI))); /* { dg-warning "specifying vector types.*deprecated" } */
/* { dg-message "use .*vector_size.* instead" "" { target *-*-* } .-1 } */

#define LIT _Literal (uint32_t)

vnx2si
__GIMPLE (startwith ("expand")) f (void)
{
  vnx2si v;

  v = _Literal (vnx2si){LIT 0x12345678};

  return v;
}

/* { dg-final { scan-assembler-times {(\tldr\ts[0-9]+,|\tfmov\ts[0-9]+, w[0-9]+)} 1 } } */

/* It is less efficient to broadcast a short repeated vector and then select
   only the low elements.  */
/* { dg-final { scan-assembler-not {\tld1rw\t} } } */
/* { dg-final { scan-assembler-not {\tsel\tz[0-9]+} } } */
