/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */
/* { dg-skip-if "no optimizations" { *-*-* } { "-O0" } { "" } } */

#include <arm_neon.h>

int64x1_t foo (int64x1_t a)
{
  return vshl_s64 (a, vdup_n_s64(-6));
}

/* { dg-final { scan-assembler-times {\tsshr\t.+, 6} 1 } } */
