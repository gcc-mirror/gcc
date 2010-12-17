/* Test the `vset_lane_s64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main (void)
{
  uint64x1_t out_uint64x1_t = 0;
  uint64_t arg0_uint64_t = 0xf00f00f00LL;
  uint64x1_t arg1_uint64x1_t = (uint64x1_t) 0xdeadbeefbadf00dLL;

  out_uint64x1_t = vset_lane_u64 (arg0_uint64_t, arg1_uint64x1_t, 0);
  if ((uint64_t)out_uint64x1_t != arg0_uint64_t)
    abort();
  return 0;
}
