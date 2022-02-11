/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

uint8x8_t foo (uint8x8_t a)
{
  return vshr_n_u8 (a, 2);
}

/* { dg-final { scan-assembler-times {\tushr\t.+, 2} 1 } } */
