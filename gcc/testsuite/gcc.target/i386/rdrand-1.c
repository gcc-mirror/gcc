/* { dg-do compile } */
/* { dg-options "-O2 -mrdrnd -dp" } */
/* { dg-final { scan-assembler-times "rdrandhi_1" 1 } } */
/* { dg-final { scan-assembler-times "\\*movsicc_noc" 1 } } */

#include <immintrin.h>

int
foo (unsigned short *x)
{
  return _rdrand16_step (x);
}
