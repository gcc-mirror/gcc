/* { dg-do assemble { target aarch64*-*-* } } */
/* { dg-additional-options "--save-temps" } */
/* { dg-skip-if "no optimizations" { *-*-* } { "-O0" } { "" } } */

#include <arm_neon.h>

int32x4_t foo (int32x4_t x) {
  return vshlq_s32(vdupq_n_s32(1), vdupq_n_s32(10));
}

/* { dg-final { scan-assembler-not {\tsshl\t} } } */
/* { dg-final { scan-assembler-times {\tmovi\t} 1 } } */
