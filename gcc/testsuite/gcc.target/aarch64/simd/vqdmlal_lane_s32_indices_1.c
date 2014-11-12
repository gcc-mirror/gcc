/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_b = vcreate_u64 (0x5555666677778888ULL);
  uint64x1_t base_c = vcreate_u64 (0x9999aaaabbbbccccULL);
  uint64x1_t base_a = vcreate_u64 (0x1111222233334444ULL);
  uint64x2_t baseq_a = vcombine_u64 (base_a, base_b);
  int32x2_t int32x2_b = vreinterpret_s32_u64 (base_b);
  int32x2_t int32x2_c = vreinterpret_s32_u64 (base_c);
  int64x2_t int64x2_a = vreinterpretq_s64_u64 (baseq_a);

  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  vqdmlal_lane_s32 (int64x2_a, int32x2_b, int32x2_c, -1);
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  vqdmlal_lane_s32 (int64x2_a, int32x2_b, int32x2_c, 2);
}
