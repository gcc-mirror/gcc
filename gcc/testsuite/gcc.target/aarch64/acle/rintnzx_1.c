/* Test the __rint[32,64][z,x] intrinsics.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.5-a" } */

#include <arm_acle.h>

#ifdef __ARM_FEATURE_FRINT
float
foo_32z_f32_scal (float a)
{
  return __rint32zf (a);
}

/* { dg-final { scan-assembler-times "frint32z\ts\[0-9\]+, s\[0-9\]+\n" 1 } } */

double
foo_32z_f64_scal (double a)
{
  return __rint32z (a);
}

/* { dg-final { scan-assembler-times "frint32z\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float
foo_32x_f32_scal (float a)
{
  return __rint32xf (a);
}

/* { dg-final { scan-assembler-times "frint32x\ts\[0-9\]+, s\[0-9\]+\n" 1 } } */

double
foo_32x_f64_scal (double a)
{
  return __rint32x (a);
}

/* { dg-final { scan-assembler-times "frint32x\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float
foo_64z_f32_scal (float a)
{
  return __rint64zf (a);
}

/* { dg-final { scan-assembler-times "frint64z\ts\[0-9\]+, s\[0-9\]+\n" 1 } } */

double
foo_64z_f64_scal (double a)
{
  return __rint64z (a);
}

/* { dg-final { scan-assembler-times "frint64z\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float
foo_64x_f32_scal (float a)
{
  return __rint64xf (a);
}

/* { dg-final { scan-assembler-times "frint64x\ts\[0-9\]+, s\[0-9\]+\n" 1 } } */

double
foo_64x_f64_scal (double a)
{
  return __rint64x (a);
}

/* { dg-final { scan-assembler-times "frint64x\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

#endif
