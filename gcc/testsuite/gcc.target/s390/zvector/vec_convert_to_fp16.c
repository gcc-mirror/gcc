/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch14 -mzvector" } */

#include <vecintrin.h>

vector unsigned short int
test_vec_convert_to_fp16 (vector unsigned short int a)
{
  return vec_convert_to_fp16 (a, 0);
}

/* { dg-final { scan-assembler-times "vcfn\t" 1 } } */
