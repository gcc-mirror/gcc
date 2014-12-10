/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_a = vcreate_u64 (0x1111222233334444ULL);
  uint64x1_t base_b = vcreate_u64 (0x5555666677778888ULL);
  int16x4_t int16x4_a = vreinterpret_s16_u64 (base_a);
  int16x4_t int16x4_b = vreinterpret_s16_u64 (base_b);

  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  vqdmulh_lane_s16 (int16x4_a, int16x4_b, -1);
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  vqdmulh_lane_s16 (int16x4_a, int16x4_b, 4);
}
