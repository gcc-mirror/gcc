/* { dg-do compile } */

#include <arm_sme.h>

#pragma GCC target ("arch=armv9-a+sme")

void
f1 (svbool_t pg, svfloat64_t f64) __arm_streaming __arm_inout("za")
{
  svmopa_za64_m (0, pg, pg, f64, f64); /* { dg-error {ACLE function 'svmopa_za64_f64_m' requires ISA extension 'sme-f64f64'} } */
}
