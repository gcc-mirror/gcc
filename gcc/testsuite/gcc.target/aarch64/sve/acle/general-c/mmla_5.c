/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve" } */

#include <arm_sve.h>

void
f1 (svfloat32_t f32)
{
  svmmla (f32, f32, f32); /* { dg-error {ACLE function 'svmmla_f32' requires ISA extension 'f32mm'} } */
}
