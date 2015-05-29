/* Test vbslq_f64 can be folded.  */
/* { dg-do assemble } */
/* { dg-options "--save-temps -O3" } */

#include <arm_neon.h>

/* Should fold out one half of the BSL, leaving just a BIC.  */

float32x4_t
half_fold_me (uint32x4_t mask)
{
  float32x4_t a = {0.0, 0.0, 0.0, 0.0};
  float32x4_t b = {2.0, 4.0, 8.0, 16.0};
  return vbslq_f32 (mask, a, b);

}

/* { dg-final { scan-assembler-not "bsl\\tv" } } */
/* { dg-final { scan-assembler-not "bit\\tv" } } */
/* { dg-final { scan-assembler-not "bif\\tv" } } */
/* { dg-final { scan-assembler "bic\\tv" } } */


