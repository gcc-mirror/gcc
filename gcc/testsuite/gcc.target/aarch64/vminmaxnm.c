/* { dg-do compile } */
/* { dg-options "-O2" } */

#include "arm_neon.h"

/* For each of these intrinsics, we map directly to an unspec in RTL.
   We're just using the argument directly and returning the result, so we
   can precisely specify the exact instruction pattern and register
   allocations we expect.  */

float64x1_t
test_vmaxnm_f64 (float64x1_t a, float64x1_t b)
{
  /* { dg-final { scan-assembler-times "fmaxnm\td0, d0, d1" 1 } } */
  return vmaxnm_f64 (a, b);
}

float64x1_t
test_vminnm_f64 (float64x1_t a, float64x1_t b)
{
  /* { dg-final { scan-assembler-times "fminnm\td0, d0, d1" 1 } } */
  return vminnm_f64 (a, b);
}

float64x1_t
test_vmax_f64 (float64x1_t a, float64x1_t b)
{
  /* { dg-final { scan-assembler-times "fmax\td0, d0, d1" 1 } } */
  return vmax_f64 (a, b);
}

float64x1_t
test_vmin_f64 (float64x1_t a, float64x1_t b)
{
  /* { dg-final { scan-assembler-times "fmin\td0, d0, d1" 1 } } */
  return vmin_f64 (a, b);
}