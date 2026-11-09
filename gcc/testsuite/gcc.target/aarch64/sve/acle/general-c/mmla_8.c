/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve" } */

#include <arm_sve.h>

void
f1 (svfloat16_t f16)
{
  svmmla (f16, f16, f16); /* { dg-error {ACLE function 'svmmla_f16' requires ISA extension 'sve2p2'} } */
}
