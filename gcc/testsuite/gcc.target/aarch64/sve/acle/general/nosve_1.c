/* { dg-options "-march=armv8-a" } */

#pragma GCC aarch64 "arm_sve.h"

void
f (svbool_t *x, svint8_t *y)
{
  *x = svptrue_b8 (); /* { dg-error {ACLE function '(svbool_t svptrue_b8\(\)|svptrue_b8)' requires ISA extension 'sve'} } */
  /* { dg-message {note: you can enable 'sve' using the command-line option '-march', or by using the 'target' attribute or pragma} "" { target *-*-* } .-1 } */
  *x = svptrue_b8 ();
  *x = svptrue_b8 ();
  *x = svptrue_b8 ();
  *x = svptrue_b8 ();
  *x = svptrue_b8 ();
  *x = svptrue_b8 ();
  *y = svadd_m (*x, *y, 1);
}
