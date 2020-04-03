/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a+sve" } */

#include <arm_sve.h>

void
f1 (svint32_t s32, svint8_t s8)
{
  svmmla_s32 (s32, s8, s8); /* { dg-error {ACLE function 'svmmla_s32' requires ISA extension 'i8mm'} } */
}
