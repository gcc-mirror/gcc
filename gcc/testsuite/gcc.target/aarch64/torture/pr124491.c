/* { dg-do compile } */
/* { dg-options "-march=armv8.2-a+sve -msve-vector-bits=256" } */
#include <arm_sve.h>
svint64_t foo(long x, long y) {
  return svdupq_n_s64(x, y);
}
