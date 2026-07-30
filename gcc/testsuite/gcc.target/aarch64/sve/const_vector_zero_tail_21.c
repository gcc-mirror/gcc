/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -msve-vector-bits=scalable" } */

/* A scalable vector constant with 9 explicitly initialized 8-bit integer
   elements cannot currently be loaded with an Advanced SIMD quadword load or
   GPR-to-SIMD move, but it's still a boundary condition worth testing.
 */

#include <arm_sve.h>

#define LIT _Literal (uint8_t)

svuint8_t
__GIMPLE (startwith ("expand")) f (void)
{
  svuint8_t v;

  v = _Literal (svuint8_t){
    LIT 1, LIT 2, LIT 1, LIT 2,
    LIT 1, LIT 2, LIT 1, LIT 2,
    LIT 1
  };

  return v;
}

/* The general-purpose expansion broadcasts a short repeated vector and
   then selects only the low elements.  */
/* { dg-final { scan-assembler-times {\tld1rqb\t} 1 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+} 1 } } */
