/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-O3 --save-temps" } */

#include <arm_neon.h>

void foo (void)
{
  bfloat16_t (); /* { dg-bogus {invalid conversion to type 'bfloat16_t'} "" } */
  bfloat16_t a = bfloat16_t(); /* { dg-bogus {invalid conversion to type 'bfloat16_t'} "" } */
  bfloat16_t (0x1234);
  bfloat16_t (0.1);
}
