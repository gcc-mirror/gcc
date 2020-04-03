/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-additional-options "-march=armv8.2-a+nobf16" } */

#include <arm_neon.h>

bfloat16_t test_bfcvt (float32_t a)
{
  /* { dg-error "inlining failed .* 'vcvth_bf16_f32" "" { target *-*-* } 0 } */
  return vcvth_bf16_f32 (a);
}
