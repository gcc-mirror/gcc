/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mrdrnd " } */
/* { dg-final { scan-assembler "rdrand\[ \t]+(%|)rax" } } */
/* { dg-final { scan-assembler "jnc\[ \t]+" } } */

#include <immintrin.h>

unsigned long long
read_rdrand64 (void)
{
  return _rdrand_u64 ();
}
