/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -msse2 -mno-mmx" } */
/* { dg-final { scan-assembler "movnti" } } */
/* { dg-final { scan-assembler-not "movntq" } } */
/* { dg-final { scan-assembler-not "%mm" } } */

#include <xmmintrin.h>

void
foo (__m64 *p, __m64 i)
{
  _mm_stream_pi (p, i);
}
