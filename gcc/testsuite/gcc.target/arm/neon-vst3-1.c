/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

uint32_t buffer[64];

void __attribute__((noinline))
foo (uint32_t *a)
{
  uint32x4x3_t x;

  x = vld3q_u32 (a);
  a[35] = 1;
  vst3q_lane_u32 (a + 32, x, 1);
}

int
main (void)
{
  foo (buffer);
  return buffer[35] != 1;
}
