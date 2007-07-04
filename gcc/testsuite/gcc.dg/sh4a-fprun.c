/* Verify that fsca and fssra yield reasonable results.  */
/* This test calls the sinf and cosf library functions for targets other
   than sh4a, but the VxWorks kernel doesn't have those functions.  */
/* { dg-do run { target { "sh*-*-*" && { ! vxworks_kernel } } } } */
/* { dg-options "-O -ffast-math" } */

#include <math.h>
#include <stdlib.h>

float sqrt_arg = 4.0f, sqrt_res = 2.0f;
float dg2rad_f;
double dg2rad_d;

void check_f (float res, float expected) {
  if (res >= expected - 0.001f && res <= expected + 0.001f)
    return;

  abort ();
}

void check_d (double res, double expected) {
  if (res >= expected - 0.001 && res <= expected + 0.001)
    return;

  abort ();
}

int main() {
  check_f (sqrtf(sqrt_arg), sqrt_res);
  dg2rad_f = dg2rad_d = atan(1) / 45;
  check_f (sinf(90*dg2rad_f), 1);
  check_f (cosf(90*dg2rad_f), 0);
  check_d (sin(-90*dg2rad_d), -1);
  check_d (cos(180*dg2rad_d), -1);
  check_d (sin(-45*dg2rad_d) * cosf(135*dg2rad_f), 0.5);
  exit (0);
}
