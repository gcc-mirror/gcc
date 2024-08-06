/* { dg-do compile } */
/* { dg-additional-options "-march=armv9-a" } */

#include "arm_neon.h"

void
test (float32x4_t a, float32x4_t b)
{
  vamaxq_f32 (a, b); /* { dg-error {ACLE function 'vamaxq_f32' requires ISA extension 'faminmax'} } */
}
