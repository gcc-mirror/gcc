/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-std=c99 -pedantic-errors -O3 --save-temps" } */

#include <arm_bf16.h>

_Complex bfloat16_t stacktest1 (_Complex bfloat16_t __a)
{
  volatile _Complex bfloat16_t b = __a;
  return b;
}

/* { dg-error {ISO C does not support plain 'complex' meaning 'double complex'} "" { target *-*-* } 8 } */
/* { dg-error {expected '=', ',', ';', 'asm' or '__attribute__' before 'stacktest1'} "" { target *-*-* } 8 } */

