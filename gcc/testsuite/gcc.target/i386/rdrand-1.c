/* { dg-do compile } */
/* { dg-options "-O2 -mrdrnd " } */
/* { dg-final { scan-assembler "rdrand\[ \t]+(%|)ax" } } */

#include <immintrin.h>

unsigned short
read_rdrand16 (void)
{
  return _rdrand_u16 ();
}
