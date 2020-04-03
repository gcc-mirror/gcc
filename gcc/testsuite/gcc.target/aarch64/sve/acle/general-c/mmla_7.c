/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve" } */

#include <arm_sve.h>

void
f1 (svfloat64_t f64)
{
  svmmla (f64, f64, f64); /* { dg-error {ACLE function 'svmmla_f64' requires ISA extension 'f64mm'} } */
}
