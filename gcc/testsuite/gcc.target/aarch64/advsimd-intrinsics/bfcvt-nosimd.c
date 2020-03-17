/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target aarch64_asm_bf16_ok } */
/* { dg-additional-options "-save-temps -march=armv8.2-a+bf16+nosimd" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */

#include <arm_neon.h>

/*
**test_bfcvt:
**	bfcvt	h0, s0
**	ret
*/
bfloat16_t test_bfcvt (float32_t a)
{
  return vcvth_bf16_f32 (a);
}
