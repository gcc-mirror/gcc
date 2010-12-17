/* Test the `vget_lane_u64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main (void)
{
  uint64_t out_uint64_t = 0;
  uint64x1_t arg0_uint64x1_t = (uint64x1_t) 0xdeadbeefbadf00dLL;

  out_uint64_t = vget_lane_u64 (arg0_uint64x1_t, 0);
  if (out_uint64_t != (uint64_t)arg0_uint64x1_t)
    abort();
  return 0;
}
