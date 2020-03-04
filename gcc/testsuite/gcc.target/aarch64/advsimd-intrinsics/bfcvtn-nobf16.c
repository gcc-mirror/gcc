/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-additional-options "-march=armv8.2-a+nobf16" } */

#include <arm_neon.h>

bfloat16x4_t test_bfcvtn (float32x4_t a)
{
  /* { dg-error "inlining failed .* 'vcvt_bf16_f32" "" { target *-*-* } 0 } */
  return vcvt_bf16_f32 (a);
}
