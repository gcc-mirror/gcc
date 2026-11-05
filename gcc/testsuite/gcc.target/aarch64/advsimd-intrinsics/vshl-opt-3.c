/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

int16x8_t foo (int16x8_t a)
{
  return vshrq_n_s16 (a, 16);
}

/* An arithmetic shift right by the element width leaves every bit equal to
   the sign bit, so it folds to a shift by one less, which in turn is just a
   comparison against zero.  */
/* { dg-final { scan-assembler-times {\tcmlt\t.+, #0} 1 } } */
