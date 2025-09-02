/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+sve2+fp8dot2" } */
#include <arm_sve.h>

/* This triggered an ICE with an unrecognizable insn due to incorrect gating of
   the insn in the backend.  */
svfloat16_t foo(svfloat16_t a, svmfloat8_t b, svmfloat8_t c, unsigned long fpm)
{
  return svdot_lane_fpm (a, b, c, 0, fpm);
}
