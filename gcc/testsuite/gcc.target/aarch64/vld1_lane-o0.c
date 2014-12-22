/* PR/63950 Test bounds checking at -O0.  */

/* { dg-options "-std=c99 -O0" } */

#include <arm_neon.h>

int
main (int argc, char **argv)
{
  int16x4_t in = vcreate_s16 (0xdeadbeef00000000ULL);
  int16_t src = 17;
  int16x4_t out = vld1_lane_s16 (&src, in, 1);
}
