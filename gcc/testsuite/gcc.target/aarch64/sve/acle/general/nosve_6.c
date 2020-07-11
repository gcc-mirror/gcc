/* { dg-options "-march=armv8-a -mgeneral-regs-only" } */

#pragma GCC aarch64 "arm_sve.h"

#pragma GCC target "+sve"

void
f (svbool_t *x, svint8_t *y)
{
  *x = svptrue_b8 (); /* { dg-error {ACLE function '(svbool_t svptrue_b8\(\)|svptrue_b8)' is incompatible with the use of '-mgeneral-regs-only'} } */
  *y = svadd_m (*x, *y, 1);
}
