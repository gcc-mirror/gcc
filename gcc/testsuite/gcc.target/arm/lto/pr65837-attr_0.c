/* { dg-lto-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-require-effective-target arm_neon_ok_no_float_abi } */
/* { dg-lto-options {{-flto -mfpu=neon}} } */

#include "arm_neon.h"

#pragma GCC target ("fpu=neon")

float32x2_t a, b, c, e;

int main()
{
  e = vmls_lane_f32 (a, b, c, 0);
  return 0;
}

