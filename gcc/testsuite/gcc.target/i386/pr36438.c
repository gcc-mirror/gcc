/* { dg-do compile } */
/* { dg-options "-O2 -mmmx" } */

#include <mmintrin.h>

extern __m64 SetS16 (unsigned short, unsigned short,
		     unsigned short, unsigned short);

void foo(__m64* dest)
{
  __m64 mask = SetS16 (0x00FF, 0xFF00, 0x0000, 0x00FF);

  mask = _mm_slli_si64(mask, 8);
  mask = _mm_slli_si64(mask, 8);

  *dest = mask;

  _mm_empty ();
}
