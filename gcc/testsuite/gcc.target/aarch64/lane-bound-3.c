/* { dg-do compile } */
/* { dg-options "-O2" } */

/* PR target/117665 */
/* __builtin_aarch64_im_lane_boundsi was causing an abnormal
   edge to the setjmp but then the builtin was folded into a nop
   and that edge was never removed but the edge was not needed in
   the first place. */

#include <arm_neon.h>

__attribute__((always_inline))
static inline
void h(uint64x2_t c, int __b) {
   /* Use vgetq_lane_u64 to get a 
     __builtin_aarch64_im_lane_boundsi */
   vgetq_lane_u64(c, __b);

  __builtin_unreachable();
}

int _setjmp();
void hh(uint64x2_t c) {
  int __b = 0;
  if (_setjmp())
    h(c, 0);
}
