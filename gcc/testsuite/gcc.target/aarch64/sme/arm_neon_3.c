// { dg-options "" }

#include <arm_neon.h>

// { dg-error {inlining failed.*'vhaddq_s32'} "" { target *-*-* } 0 }

int32x4_t
foo (int32x4_t x, int32x4_t y) [[arm::streaming]]
{
  return vhaddq_s32 (x, y);
}
