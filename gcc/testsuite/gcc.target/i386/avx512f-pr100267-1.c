/* { dg-do compile } */
/* { dg-options "-mavx512f -O2" } */
/* { dg-final { scan-assembler-times "vpexpandd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandd\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  4 } } */
/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\(]*%zmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  4 } } */
#include <immintrin.h>

int *pi32;
long long *pi64;
double *pd;
float *pf;
volatile __m512i xi32, xi64;
volatile __m512d xd;
volatile __m512 xf;

void extern
avx512f_test (void)
{
  xi32 = _mm512_mask_expand_epi32 (xi32, 0, xi32);
  xi32 = _mm512_mask_expand_epi32 (xi32, -1, xi32);
  xi32 = _mm512_mask_expand_epi32 (xi32, (1 << 8) - 1, xi32);
  xi32 = _mm512_mask_expand_epi32 (xi32, (1 << 8) + 1, xi32);

  xi32 = _mm512_mask_expandloadu_epi32 (xi32, 0, pi32);
  xi32 = _mm512_mask_expandloadu_epi32 (xi32, (1 << 16) - 1, pi32);
  xi32 = _mm512_mask_expandloadu_epi32 (xi32, (1 << 6) - 1, pi32);
  xi32 = _mm512_mask_expandloadu_epi32 (xi32, (1 << 6) + 3, pi32);

  xi64 = _mm512_mask_expand_epi64 (xi64, 0, xi64);
  xi64 = _mm512_mask_expand_epi64 (xi64, -1, xi64);
  xi64 = _mm512_mask_expand_epi64 (xi64, (1 << 3) - 1, xi64);
  xi64 = _mm512_mask_expand_epi64 (xi64, (1 << 3) + 2, xi64);

  xi64 = _mm512_mask_expandloadu_epi64 (xi64, 0, pi64);
  xi64 = _mm512_mask_expandloadu_epi64 (xi64, (1 << 8) - 1, pi64);
  xi64 = _mm512_mask_expandloadu_epi64 (xi64, (1 << 7) - 1, pi64);
  xi64 = _mm512_mask_expandloadu_epi64 (xi64, (1 << 7) + 7, pi64);

  xf = _mm512_mask_expand_ps (xf, 0, xf);
  xf = _mm512_mask_expand_ps (xf, (1 << 16) - 1, xf);
  xf = _mm512_mask_expand_ps (xf, (1 << 15) - 1, xf);
  xf = _mm512_mask_expand_ps (xf, (1 << 14) + 3, xf);

  xf = _mm512_mask_expandloadu_ps (xf, 0, pf);
  xf = _mm512_mask_expandloadu_ps (xf, -1, pf);
  xf = _mm512_mask_expandloadu_ps (xf, (1 << 13) - 1, pf);
  xf = _mm512_mask_expandloadu_ps (xf, (1 << 13) + 5, pf);

  xd = _mm512_mask_expand_pd (xd, 0, xd);
  xd = _mm512_mask_expand_pd (xd, (1 << 8) - 1, xd);
  xd = _mm512_mask_expand_pd (xd, (1 << 4) - 1, xd);
  xd = _mm512_mask_expand_pd (xd, (1 << 4) + 1, xd);

  xd = _mm512_mask_expandloadu_pd (xd, 0, pd);
  xd = _mm512_mask_expandloadu_pd (xd, -1, pd);
  xd = _mm512_mask_expandloadu_pd (xd, (1 << 5) - 1, pd);
  xd = _mm512_mask_expandloadu_pd (xd, (1 << 5), pd);
}
