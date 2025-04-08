/* { dg-do compile } */
/* { dg-options "" } */
#include <arm_neon.h>
void test()
{
  for (int L = 0; L < 4; ++L) {
    float64_t ResData[1 * 2];
    float64x1x2_t Src1;
    vst2_f64(ResData, Src1);
  }
}
