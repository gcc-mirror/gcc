/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch14 -mzvector" } */

#include <vecintrin.h>

vector float
test_vec_extend_to_fp32_lo (vector unsigned short int a)
{
  return vec_extend_to_fp32_lo (a, 0);
}

/* { dg-final { scan-assembler-times "vclfnl\t" 1 } } */
