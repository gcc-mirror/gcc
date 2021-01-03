/* { dg-do compile } */
/* { dg-options "-O2" } */

/* This maps to essentially the same gimple that is generated for
   gnat.dg/sin_cos.adb, on platforms that use the wraplf variant of
   Ada.Numerics.Aux_Float.  The value of EPSILON is not relevant to
   the test, but the test must be there to keep the conversions in
   different BBs long enough to trigger the problem that prevented the
   sincos optimization, because the arguments passed to sin and cos
   didn't get unified into a single SSA_NAME in time for sincos.  */

#include <math.h>

#define EPSILON 3.4526697709225118160247802734375e-4

static float my_sinf(float x) {
  return (float) sin ((double) x);
}

static float wrap_sinf(float x) {
  if (fabs (x) < EPSILON)
    return 0;
  return my_sinf (x);
}

static float my_cosf(float x) {
  return (float) cos ((double) x);
}

static float wrap_cosf(float x) {
  if (fabs (x) < EPSILON)
    return 1;
  return my_cosf (x);
}

float my_sin_cos(float x, float *s, float *c) {
  *s = wrap_sinf (x);
  *c = wrap_cosf (x);
}

/* { dg-final { scan-assembler "sincos\|cexp" { target *-linux-gnu* *-w64-mingw* *-*-vxworks* } } } */
