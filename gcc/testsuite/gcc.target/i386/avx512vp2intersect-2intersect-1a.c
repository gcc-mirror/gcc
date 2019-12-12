/* { dg-do compile } */
/* { dg-options "-O2 -mavx512vp2intersect" } */
/* { dg-final { scan-assembler "vp2intersectq\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[0-7\]"} } */
/* { dg-final { scan-assembler "vp2intersectd\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\]*%k\[0-7\]"} } */

#include <x86intrin.h>

__m512i a1, b1;
__m512i a2, b2;
__mmask8 m8, u8;
__mmask16 m16, u16;

int foo ()
{
  _mm512_2intersect_epi64 (a1, b1, &u8, &m8);
  _mm512_2intersect_epi32 (a2, b2, &u16, &m16);
}

