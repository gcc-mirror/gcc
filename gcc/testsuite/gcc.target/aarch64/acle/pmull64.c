/* { dg-do compile } */
/* { dg-additional-options "-march=armv8.2-a" } */

#pragma push_options
#pragma GCC target ("+aes")

#include "arm_neon.h"

int foo (poly64_t a, poly64_t b)
{
  return vgetq_lane_s32 (vreinterpretq_s32_p128 (vmull_p64 (a, b)), 0);
}

/* { dg-final { scan-assembler "\tpmull\tv" } } */
