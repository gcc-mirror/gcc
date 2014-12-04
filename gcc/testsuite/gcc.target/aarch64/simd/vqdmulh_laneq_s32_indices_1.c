/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_a = vcreate_u64 (0x1111222233334444ULL);
  uint64x1_t base_b = vcreate_u64 (0x5555666677778888ULL);
  uint64x1_t base_c = vcreate_u64 (0x9999aaaabbbbccccULL);
  uint64x2_t baseq_b = vcombine_u64 (base_b, base_c);
  int32x2_t int32x2_a = vreinterpret_s32_u64 (base_a);
  int32x4_t int32x4_b = vreinterpretq_s32_u64 (baseq_b);

  /* { dg-error "lane -1 out of range 0 - 3" "" {target *-*-*} 0 } */
  vqdmulh_laneq_s32 (int32x2_a, int32x4_b, -1);
  /* { dg-error "lane 4 out of range 0 - 3" "" {target *-*-*} 0 } */
  vqdmulh_laneq_s32 (int32x2_a, int32x4_b, 4);
}
