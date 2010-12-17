/* Test the `vorn_s64' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>

int main (void)
{
  int64x1_t out_int64x1_t = 0;
  int64x1_t arg0_int64x1_t = (int64x1_t)0xdeadbeef00000000LL;
  int64x1_t arg1_int64x1_t = (int64x1_t)(~0xdead00000000beefLL);

  out_int64x1_t = vorn_s64 (arg0_int64x1_t, arg1_int64x1_t);
  if (out_int64x1_t != (int64x1_t)0xdeadbeef0000beefLL)
    abort();
  return 0;
}
