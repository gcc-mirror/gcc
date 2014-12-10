/* Test error message when passing a non-constant value in as a lane index.  */

/* { dg-do assemble } */
/* { dg-options "-std=c99" } */

#include <arm_neon.h>

int
main (int argc, char **argv)
{
  int16x4_t in = vcreate_s16 (0xdeadbeef00000000ULL);
  /* { dg-error "must be a constant immediate" "" { target *-*-* } 0 } */
  int16x4_t out = vset_lane_s16 (65535, in, argc);
  return vget_lane_s16 (out, 0);
}
