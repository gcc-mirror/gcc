/* { dg-do compile } */
/* { dg-options "-O2 -msse3" } */
/* { dg-final { scan-assembler-not "%mm" } } */
/* PR 28825 */
#include <pmmintrin.h>
__m128 ggg(float* m)
{
  return (__m128) {m[0], m[5], m[10], m[10]};
}
