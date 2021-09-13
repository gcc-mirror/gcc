/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch14 -mzvector" } */

#include <vecintrin.h>

vector short int
test_vec_round_from_fp32 (vector float hi, vector float lo)
{
  return vec_round_from_fp32 (hi, lo, 0);
}

/* { dg-final { scan-assembler-times "vcrnf\t" 1 } } */
