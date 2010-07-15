/* { dg-do compile } */
/* { dg-options "-O -msse" } */
/* { dg-require-effective-target sse } */

#include <xmmintrin.h>

typedef unsigned short v4hi __attribute__ ((vector_size (8)));

int f(unsigned short n)
{
   __m64 vec = (__m64)(v4hi){ 0, 0, 1, n };
   __m64 hw = _mm_mulhi_pi16 (vec, vec);
   return _mm_extract_pi16 (hw, 0);
}
