/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
void
f (void)
{
  __builtin_aarch64_im_lane_boundsi (16, 4, 0);
  __builtin_aarch64_im_lane_boundsi (8, 8, 0);
}
/* GCC should be able to optimize these out before gimplification. */
/* { dg-final { scan-tree-dump-times "__builtin_aarch64_im_lane_boundsi" 0 "original" } } */
