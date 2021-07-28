/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.2-a+sve" } */

/* Check that we avoid an explicit sxth in favour of smov.  */

#include <arm_sve.h>

int foo(svint16_t a) {
  return svminv_s16(svptrue_b16(), a);
}

/* { dg-final { scan-assembler-not "sxth" } } */
