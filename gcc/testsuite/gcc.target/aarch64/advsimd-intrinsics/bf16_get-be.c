/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target stdint_types_mbig_endian } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-mbig-endian -save-temps" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/*
**test_vget_low_bf16:
**     ret
*/
bfloat16x4_t test_vget_low_bf16 (bfloat16x8_t a)
{
  return vget_low_bf16 (a);
}

/*
**test_vget_high_bf16:
**     dup	d0, v0.d\[1\]
**     ret
*/
bfloat16x4_t test_vget_high_bf16 (bfloat16x8_t a)
{
  return vget_high_bf16 (a);
}
