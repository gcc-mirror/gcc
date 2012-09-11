/* Test the `vset_lane_s8' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O0" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"
#include <stdlib.h>
#include <string.h>

int8_t x_init[8] = { 1, 2, 3, 4, 5, 6, 7, 8 };
int8_t y_init[8] = { 1, 2, 3, 16, 5, 6, 7, 8 };

int main (void)
{
  int8x8_t x = vld1_s8 (x_init);
  int8x8_t y = vld1_s8 (y_init);

  x = vset_lane_s8 (16, x, 3);
  if (memcmp (&x, &y, sizeof (x)) != 0)
    abort();
  return 0;
}
