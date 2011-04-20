/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

uint32_t buffer[12];

void __attribute__((noinline))
foo (uint32_t *a)
{
  uint32x4x3_t x;

  x = vld3q_u32 (a);
  x.val[0] = vaddq_u32 (x.val[0], x.val[1]);
  vst3q_u32 (a, x);
}

int
main (void)
{
  buffer[0] = 1;
  buffer[1] = 2;
  foo (buffer);
  return buffer[0] != 3;
}
