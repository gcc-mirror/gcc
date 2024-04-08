/* { dg-require-effective-target loongarch_sx_as } */
/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { loongarch*-linux-* } } } */
/* { dg-lto-options { {-mlsx } } } */
/* { dg-suppress-ld-options { -mlsx } } */

#include <lsxintrin.h>

int main (void)
{
  __m128i a, b, c;
  c = __lsx_vand_v (a, b);
  return 0;
} 
