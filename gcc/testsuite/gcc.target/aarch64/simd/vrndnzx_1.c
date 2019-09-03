/* Test the vrnd[32,64][z,x] intrinsics.  */

/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.5-a" } */

#include "arm_neon.h"

#ifdef __ARM_FEATURE_FRINT

float32x2_t
foo_32z (float32x2_t a)
{
  return vrnd32z_f32 (a);
}

/* { dg-final { scan-assembler-times "frint32z\tv\[0-9\]+\.2s, v\[0-9\]+\.2s\n" 1 } } */

float32x4_t
foo_32z_q (float32x4_t a)
{
  return vrnd32zq_f32 (a);
}

/* { dg-final { scan-assembler-times "frint32z\tv\[0-9\]+\.4s, v\[0-9\]+\.4s\n" 1 } } */

float64x1_t
foo_32z_f64 (float64x1_t a)
{
  return vrnd32z_f64 (a);
}

/* { dg-final { scan-assembler-times "frint32z\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float64x2_t
foo_32z_q_f64 (float64x2_t a)
{
  return vrnd32zq_f64 (a);
}

/* { dg-final { scan-assembler-times "frint32z\tv\[0-9\]+\.2d, v\[0-9\]+\.2d\n" 1 } } */

float32x2_t
foo_32x (float32x2_t a)
{
  return vrnd32x_f32 (a);
}

/* { dg-final { scan-assembler-times "frint32x\tv\[0-9\]+\.2s, v\[0-9\]+\.2s\n" 1 } } */

float32x4_t
foo_32x_q (float32x4_t a)
{
  return vrnd32xq_f32 (a);
}

/* { dg-final { scan-assembler-times "frint32x\tv\[0-9\]+\.4s, v\[0-9\]+\.4s\n" 1 } } */

float64x1_t
foo_32x_f64 (float64x1_t a)
{
  return vrnd32x_f64 (a);
}

/* { dg-final { scan-assembler-times "frint32x\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float64x2_t
foo_32x_q_f64 (float64x2_t a)
{
  return vrnd32xq_f64 (a);
}

/* { dg-final { scan-assembler-times "frint32x\tv\[0-9\]+\.2d, v\[0-9\]+\.2d\n" 1 } } */

float32x2_t
foo_64z (float32x2_t a)
{
  return vrnd64z_f32 (a);
}

/* { dg-final { scan-assembler-times "frint64z\tv\[0-9\]+\.2s, v\[0-9\]+\.2s\n" 1 } } */

float32x4_t
foo_64z_q (float32x4_t a)
{
  return vrnd64zq_f32 (a);
}

/* { dg-final { scan-assembler-times "frint64z\tv\[0-9\]+\.4s, v\[0-9\]+\.4s\n" 1 } } */

float64x1_t
foo_64z_f64 (float64x1_t a)
{
  return vrnd64z_f64 (a);
}

/* { dg-final { scan-assembler-times "frint64z\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float64x2_t
foo_64z_q_f64 (float64x2_t a)
{
  return vrnd64zq_f64 (a);
}

/* { dg-final { scan-assembler-times "frint64z\tv\[0-9\]+\.2d, v\[0-9\]+\.2d\n" 1 } } */

float32x2_t
foo_64x (float32x2_t a)
{
  return vrnd64x_f32 (a);
}

/* { dg-final { scan-assembler-times "frint64x\tv\[0-9\]+\.2s, v\[0-9\]+\.2s\n" 1 } } */

float32x4_t
foo_64x_q (float32x4_t a)
{
  return vrnd64xq_f32 (a);
}

/* { dg-final { scan-assembler-times "frint64x\tv\[0-9\]+\.4s, v\[0-9\]+\.4s\n" 1 } } */

float64x1_t
foo_64x_f64 (float64x1_t a)
{
  return vrnd64x_f64 (a);
}

/* { dg-final { scan-assembler-times "frint64x\td\[0-9\]+, d\[0-9\]+\n" 1 } } */

float64x2_t
foo_64x_q_f64 (float64x2_t a)
{
  return vrnd64xq_f64 (a);
}

/* { dg-final { scan-assembler-times "frint64x\tv\[0-9\]+\.2d, v\[0-9\]+\.2d\n" 1 } } */
#endif
