/* Test vbslq_u64 can be folded.  */
/* { dg-do assemble } */
/* { dg-options "--save-temps -O3" } */
#include <arm_neon.h>

/* Folds to BIC.  */

int32x4_t
half_fold_int (uint32x4_t mask)
{
  int32x4_t a = {0, 0, 0, 0};
  int32x4_t b = {2, 4, 8, 16};
  return vbslq_s32 (mask, a, b);
}

/* { dg-final { scan-assembler-not "bsl\\tv" } } */
/* { dg-final { scan-assembler-not "bit\\tv" } } */
/* { dg-final { scan-assembler-not "bif\\tv" } } */
/* { dg-final { scan-assembler "bic\\tv" } } */


