/* { dg-do compile } */
/* { dg-options "-march=x86-64-v4 -O2" } */
/* { dg-final { scan-assembler-times "vpbroadcastb" 1 } } */

#include <immintrin.h>

extern __m512i sinkz;
extern __m256i sinky;
extern char f;

void
foo(char c, int x)
{
  c += f;
  sinkz = _mm512_set1_epi8(c);
  if (x == 2)
    f += 3;
  sinky = _mm256_set1_epi8(c);
}
