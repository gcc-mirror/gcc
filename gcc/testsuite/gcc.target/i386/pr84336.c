/* PR target/84336 */
/* { dg-do compile } */
/* { dg-options "-O0 -ftree-ter -mavx512f" } */

#include <x86intrin.h>

struct S { __m512i h; } b;

__m512
foo (__m512 a, __mmask16 c, __m512 d)
{
  return _mm512_mask2_permutex2var_ps (a, b.h, c, d);
}
