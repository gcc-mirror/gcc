/* Test the `vsub_s64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main (void)
{
  int64x1_t out_int64x1_t = 0;
  int64x1_t arg0_int64x1_t = (int64x1_t)0xdeadbeefdeadbeefLL;
  int64x1_t arg1_int64x1_t = (int64x1_t)0x0000beefdead0000LL;

  out_int64x1_t = vsub_s64 (arg0_int64x1_t, arg1_int64x1_t);
  if (out_int64x1_t != (int64x1_t)0xdead00000000beefLL)
    abort();
  return 0;
}
