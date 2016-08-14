/* PR target/76342 */
/* { dg-do compile } */
/* { dg-options "-mavx512f" } */

#include <immintrin.h>

__m512i
test()
{
  return _mm512_undefined_epi32 ();
}
