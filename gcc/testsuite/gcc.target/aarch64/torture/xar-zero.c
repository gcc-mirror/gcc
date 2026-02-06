/* PR target/123584.  */
/* { dg-do compile } */
/* { dg-options "-march=armv8.2-a+sha3" } */

#include <arm_neon.h>
uint64x2_t
simde_vld1q_u64(uint64x2_t simde_vld1q_u64_a, uint64x2_t simde_vld1q_u64_b) {
  return vxarq_u64(simde_vld1q_u64_a, simde_vld1q_u64_b, 0);
}
