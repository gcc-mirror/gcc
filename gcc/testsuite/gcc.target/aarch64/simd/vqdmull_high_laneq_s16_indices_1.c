/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_a = vcreate_u64 (0x1111222233334444ULL);
  uint64x1_t base_b = vcreate_u64 (0x5555666677778888ULL);
  uint64x2_t baseq_a = vcombine_u64 (base_a, base_b);
  uint64x1_t base_c = vcreate_u64 (0x9999aaaabbbbccccULL);
  uint64x2_t baseq_b = vcombine_u64 (base_b, base_c);
  int16x8_t int16x8_a = vreinterpretq_s16_u64 (baseq_a);
  int16x8_t int16x8_b = vreinterpretq_s16_u64 (baseq_b);

  /* { dg-error "lane -1 out of range 0 - 7" "" {target *-*-*} 0 } */
  vqdmull_high_laneq_s16 (int16x8_a, int16x8_b, -1);
  /* { dg-error "lane 8 out of range 0 - 7" "" {target *-*-*} 0 } */
  vqdmull_high_laneq_s16 (int16x8_a, int16x8_b, 8);
}
