/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

int32x4_t foo (int32x4_t x) {
  return vshlq_s32(x, vdupq_n_s32(256));
}

/* { dg-final { scan-assembler-times {\tsshl\t.+, v[0-9].4s} 1 } } */
