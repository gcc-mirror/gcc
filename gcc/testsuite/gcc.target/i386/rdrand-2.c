/* { dg-do compile } */
/* { dg-options "-O2 -mrdrnd -dp" } */
/* { dg-final { scan-assembler-times "rdrandsi" 1 } } */
/* { dg-final { scan-assembler-times "\\*movsicc_noc" 1 } } */

#include <immintrin.h>

int
foo (unsigned int *x)
{
  return _rdrand32_step (x);
}
