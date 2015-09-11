// { dg-do compile { target "aarch64*-*-*" } }
#include <arm_neon.h>
int
search_line_fast (uint32x2_t t)
{
  return vget_lane_u32 (t, 0);
}

