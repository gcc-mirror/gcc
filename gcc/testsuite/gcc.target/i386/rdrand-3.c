/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mrdrnd -dp" } */
/* { dg-final { scan-assembler-times "rdranddi_1" 1 } } */
/* { dg-final { scan-assembler-times "\\*movsicc_noc" 1 } } */

#include <immintrin.h>

int
foo (unsigned long long *x)
{
  return _rdrand64_step (x);
}
