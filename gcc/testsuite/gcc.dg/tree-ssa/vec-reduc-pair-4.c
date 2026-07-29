/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -ftrapping-math -fdump-tree-optimized" } */

#include <arm_neon.h>

__attribute__((optimize ("associative-math,no-signed-zeros")))
double
f (float64x2_t a, float64x2_t b)
{
  return vaddvq_f64 (a) - vaddvq_f64 (b);
}

/* { dg-final { scan-tree-dump-times "REDUC_PLUS" 2 "optimized" } } */
