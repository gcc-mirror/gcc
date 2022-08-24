/* PR target/99464 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-msse2" { target i?86-*-* } } */

#pragma GCC target("arch=cannonlake")

#include <immintrin.h>

volatile __m128i x;

void extern
sha_test (void)
{
  x = _mm_sha1msg1_epu32 (x, x);
}
