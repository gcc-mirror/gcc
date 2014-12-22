/* { dg-do run } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

extern void abort (void);

int
main (int argc, char **argv)
{
  int8x8_t a = vabs_s8 (vdup_n_s8 (-128)); /* Should all be -128.  */
  uint8x8_t b = vcltz_s8 (a); /* Should all be true i.e. -1. */
  if (vget_lane_u8 (b, 1))
    return 0;
  abort ();
}

