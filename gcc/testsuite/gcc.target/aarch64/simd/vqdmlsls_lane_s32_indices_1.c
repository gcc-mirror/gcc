/* { dg-do assemble } */
/* { dg-options "-std=c99" } */
#include <arm_neon.h>

int
main (int argc, char **argv)
{
  uint64x1_t base_c = vcreate_u64 (0x9999aaaabbbbccccULL);
  int64_t int64_a = 0x1111222233334444LL;
  int32_t int32_b = 0xcafebabe;
  int32x2_t int32x2_c = vreinterpret_s32_u64 (base_c);

  /* { dg-error "lane -1 out of range 0 - 1" "" {target *-*-*} 0 } */
  vqdmlsls_lane_s32 (int64_a, int32_b, int32x2_c, -1);
  /* { dg-error "lane 2 out of range 0 - 1" "" {target *-*-*} 0 } */
  vqdmlsls_lane_s32 (int64_a, int32_b, int32x2_c, 2);
}

