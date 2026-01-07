/* { dg-do "run" } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint64x1_t __attribute__((noinline,noclone)) foo() {
  uint64x2_t v36 = vdupq_n_u64(0x2020000012345678);
  uint64x1_t v48 = vget_low_u64(v36);
  uint64x1_t v50 = vadd_u64(v48, v48);
  return vpadal_u32(v50, vdup_n_u32(0));
}

int main() {
  if (vget_lane_u64 (foo(), 0) != 0x404000002468acf0)
    __builtin_abort ();
  return 0;
}
