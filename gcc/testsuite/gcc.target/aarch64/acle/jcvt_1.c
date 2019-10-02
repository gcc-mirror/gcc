/* Test the __jcvt ACLE intrinsic.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.3-a" } */

#include <arm_acle.h>

#ifdef __ARM_FEATURE_JCVT
int32_t
test_jcvt (double a)
{
  return __jcvt (a);
}
#endif

/* { dg-final { scan-assembler-times "fjcvtzs\tw\[0-9\]+, d\[0-9\]+\n" 1 } } */
