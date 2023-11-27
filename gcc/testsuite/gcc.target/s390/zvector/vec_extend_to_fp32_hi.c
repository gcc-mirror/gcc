/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch14 -mzvector" } */

#include <vecintrin.h>

vector float
test_vec_extend_to_fp32_hi (vector unsigned short int a)
{
  return vec_extend_to_fp32_hi (a, 0);
}

/* { dg-final { scan-assembler-times "vclfnh\t" 1 } } */
