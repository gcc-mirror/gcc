/* PR target/94308 */
/* { dg-do compile } */
/* { dg-options "-O2 -mfpmath=sse -mavx2 -mfma" } */

#include <x86intrin.h>

void
foo (float *x, const float *y, const float *z, unsigned int w)
{
  unsigned int a;
  const unsigned int b = w / 8;
  const float *c = y;
  const float *d = z;
  __m256 e = _mm256_setzero_ps ();
  __m256 f, g;
  for (a = 0; a < b; a++)
    {
      f = _mm256_loadu_ps (c);
      g = _mm256_loadu_ps (d);
      c += 8;
      d += 8;
      e = _mm256_fmadd_ps (f, g, e);
    }
  __attribute__ ((aligned (32))) float h[8];
  _mm256_storeu_ps (h, e);
  _mm256_zeroupper ();
  float i = h[0] + h[1] + h[2] + h[3] + h[4] + h[5] + h[6] + h[7];
  for (a = b * 8; a < w; a++)
    i += (*c++) * (*d++);
  *x = i;
}
