/* { dg-options "-march=armv8-a" } */

#pragma GCC aarch64 "arm_sve.h"

void __attribute__ ((target("+sve")))
f (svbool_t *x, svint8_t *y)
{
  *x = svptrue_b8 ();
  *y = svadd_m (*x, *y, 1);
}

/* { dg-final { scan-assembler {\tadd\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} } } */
