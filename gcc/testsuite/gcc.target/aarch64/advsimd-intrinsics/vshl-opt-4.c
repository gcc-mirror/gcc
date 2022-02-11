/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

int64x1_t foo (int64x1_t a)
{
  return vshl_s64 (a, vdup_n_s64(80));
}

/* { dg-final { scan-assembler-times {\tsshl\t.+, d[0-9]+} 1 } } */
