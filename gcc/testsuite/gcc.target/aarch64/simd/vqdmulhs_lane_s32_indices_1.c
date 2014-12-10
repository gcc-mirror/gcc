/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_b = vcreate_u64 (0x5555666677778888ULL);
  int32_t int32_a = 0xdeadbeef;
  int32x2_t int32x2_b = vreinterpret_s32_u64 (base_b);

  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  vqdmulhs_lane_s32 (int32_a, int32x2_b, -1);
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  vqdmulhs_lane_s32 (int32_a, int32x2_b, 2);
}
