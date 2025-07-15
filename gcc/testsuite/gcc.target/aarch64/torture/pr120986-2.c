/* { dg-do compile } */
/* { dg-options "-march=armv8.2-a+sve2+fp8dot2" } */
#include <arm_sve.h>
svfloat16_t foo(svfloat16_t a, svmfloat8_t b, svmfloat8_t c)
{
  return svdot_lane_fpm (a, b, c, 0, 0);
}
