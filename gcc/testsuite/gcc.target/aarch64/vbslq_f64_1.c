/* Test vbslq_f64 can be folded.  */
/* { dg-do assemble } */
/* { dg-options "--save-temps -O3" } */

#include <arm_neon.h>

/* Folds to ret.  */

float32x4_t
fold_me (float32x4_t a, float32x4_t b)
{
  uint32x4_t mask = {-1, -1, -1, -1};
  return vbslq_f32 (mask, a, b);
}

/* { dg-final { scan-assembler-not "bsl\\tv" } } */
/* { dg-final { scan-assembler-not "bit\\tv" } } */
/* { dg-final { scan-assembler-not "bif\\tv" } } */


