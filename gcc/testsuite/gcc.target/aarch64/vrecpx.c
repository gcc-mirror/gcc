/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include <arm_neon.h>
#include <math.h>
#include <stdlib.h>

float32_t in_f[] =
{2.0, 4.0, 8.0, 16.0, 1.0, 0.5, 0.25, 0.125};
float32_t rec_f[] =
{1.0, 0.5, 0.25, 0.125, 2.0, 4.0, 8.0, 16.0};
float64_t in_d[] =
{2.0, 4.0, 8.0, 16.0, 1.0, 0.5, 0.25, 0.125};
float32_t rec_d[] =
{1.0, 0.5, 0.25, 0.125, 2.0, 4.0, 8.0, 16.0};

int
test_frecpx_float32_t (void)
{
  int i = 0;
  int ret = 1;
  for (i = 0; i < 8; i++)
    ret &= fabs (vrecpxs_f32 (in_f[i]) - rec_f[i]) < 0.001;

  return ret;
}

/* { dg-final { scan-assembler "frecpx\\ts\[0-9\]+, s\[0-9\]+" } } */

int
test_frecpx_float64_t (void)
{
  int i = 0;
  int ret = 1;
  for (i = 0; i < 8; i++)
    ret &= fabs (vrecpxd_f64 (in_d[i]) - rec_d[i]) < 0.001;

  return ret;
}

/* { dg-final { scan-assembler "frecpx\\td\[0-9\]+, d\[0-9\]+" } } */

int
main (int argc, char **argv)
{
  if (!test_frecpx_float32_t ())
    abort ();
  if (!test_frecpx_float64_t ())
    abort ();

  return 0;
}

