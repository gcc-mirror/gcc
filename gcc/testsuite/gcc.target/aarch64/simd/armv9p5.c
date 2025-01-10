/* { dg-do compile } */
/* { dg-additional-options "-march=armv9.5-a" } */

#include "arm_neon.h"

int8x16_t foo (int8x16_t table, uint8x16_t indicies)
{
  return vluti2q_laneq_s8 (table, indicies, 1);
}

float32x4_t bar (float32x4_t a, float32x4_t b)
{
  return vaminq_f32 (a, b);
}
