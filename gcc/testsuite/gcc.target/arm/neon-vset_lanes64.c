/* Test the `vset_lane_s64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main (void)
{
  int64x1_t out_int64x1_t = 0;
  int64_t arg0_int64_t = 0xf00f00f00LL;
  int64x1_t arg1_int64x1_t = (int64x1_t) 0xdeadbeefbadf00dLL;

  out_int64x1_t = vset_lane_s64 (arg0_int64_t, arg1_int64x1_t, 0);
  if ((int64_t)out_int64x1_t != arg0_int64_t)
    abort();
  return 0;
}
