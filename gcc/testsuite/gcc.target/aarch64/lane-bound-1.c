/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
#include <arm_neon.h>

void
f (float32x4_t **ptr)
{
  float32x4_t res = vsetq_lane_f32 (0.0f, **ptr, 0);
  **ptr = res;
}
/* GCC should be able to remove the call to "__builtin_aarch64_im_lane_boundsi"
   and optimize out the second load from *ptr.  */
/* { dg-final { scan-tree-dump-times "__builtin_aarch64_im_lane_boundsi" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times " = \\\*ptr_" 1 "optimized" } } */
