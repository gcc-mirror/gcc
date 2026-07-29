/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -fassociative-math -fno-signed-zeros -fno-trapping-math -fdump-tree-optimized" } */

#include <arm_neon.h>

double
f (float64x2_t a, float64x2_t b)
{
  return vaddvq_f64 (a) - vaddvq_f64 (b);
}

/* { dg-final { scan-tree-dump-times "REDUC_PLUS" 1 "optimized" } } */
