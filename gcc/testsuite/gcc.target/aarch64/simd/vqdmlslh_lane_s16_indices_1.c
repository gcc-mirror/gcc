/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_c = vcreate_u64 (0x9999aaaabbbbccccULL);
  int16_t int16_b = 0x5678;
  int16x4_t int16x4_c = vreinterpret_s16_u64 (base_c);
  int32_t int32_a = 0xdeadbeef;

  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  vqdmlslh_lane_s16 (int32_a, int16_b, int16x4_c, -1);
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  vqdmlslh_lane_s16 (int32_a, int16_b, int16x4_c, 4);
}
