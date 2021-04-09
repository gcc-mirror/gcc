/* PR target/98670 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1" } */

#include <x86intrin.h>

void foo (__m128i);
int a[6];

void
bar (void)
{
  __m128i d = *(__m128i *) (a + 2);
  __m128i e = _mm_unpacklo_epi16 (d, (__m128i) {});
  foo (e);
}
