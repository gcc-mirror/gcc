/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

int16x8_t foo (int16x8_t a)
{
  return vshrq_n_s16 (a, 16);
}

/* { dg-final { scan-assembler-times {\tsshr\t.+, 16} 1 } } */
