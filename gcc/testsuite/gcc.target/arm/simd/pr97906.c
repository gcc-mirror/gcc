/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint32x2_t f1(float32x2_t a, float32x2_t b)
{
  return vabs_f32 (a) >= vabs_f32 (b);
}

/* { dg-final { scan-assembler-times {\tvacle.f32\td[0-9]+, d[0-9]+, d[0-9]+} 1 } } */
/* { dg-final { scan-assembler-not "vabs" } } */
