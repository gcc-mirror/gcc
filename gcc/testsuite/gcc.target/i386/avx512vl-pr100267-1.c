/* { dg-do compile } */
/* { dg-options "-mavx512f -mavx512vl -O2" } */
/* { dg-final { scan-assembler-times "vpexpandd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandd\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%ymm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  4 } } */
/* { dg-final { scan-assembler-times "(?:vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+\{%k\[1-7\]\}|blend\[a-z]+\[ \\t\]+\[^\{\n\(]*%ymm\[0-9\]+)(?:\n|\[ \\t\]+#)"  4 } } */
/* { dg-final { scan-assembler-times "vpexpandd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandd\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vpexpandq\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandpd\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */
/* { dg-final { scan-assembler-times "vexpandps\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  1 } } */

/* { dg-final { scan-assembler-times "vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\]*\\(\[^\n\]*%xmm\[0-9\]+\{%k\[1-7\]\}(?:\n|\[ \\t\]+#)"  4 } } */
/* { dg-final { scan-assembler-times "(?:vmov\[a-z0-9\]*\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+\{%k\[1-7\]\}|(?:blend\[a-z]+|movsd)\[ \\t\]+\[^\{\n\(]*%xmm\[0-9\]+)(?:\n|\[ \\t\]+#)"  4 } } */

#include <immintrin.h>

int *pi32;
long long *pi64;
double *pd;
float *pf;
volatile __m256i xi32, xi64;
volatile __m256d xd;
volatile __m256 xf;

volatile __m128i xi32_128, xi64_128;
volatile __m128d xd_128;
volatile __m128 xf_128;

void extern
avx512vl_test (void)
{
  xi32 = _mm256_mask_expand_epi32 (xi32, 0, xi32);
  xi32 = _mm256_mask_expand_epi32 (xi32, -1, xi32);
  xi32 = _mm256_mask_expand_epi32 (xi32, (1 << 4) - 1, xi32);
  xi32 = _mm256_mask_expand_epi32 (xi32, (1 << 4) + 1, xi32);

  xi32 = _mm256_mask_expandloadu_epi32 (xi32, 0, pi32);
  xi32 = _mm256_mask_expandloadu_epi32 (xi32, (1 << 8) - 1, pi32);
  xi32 = _mm256_mask_expandloadu_epi32 (xi32, (1 << 6) - 1, pi32);
  xi32 = _mm256_mask_expandloadu_epi32 (xi32, (1 << 6) + 3, pi32);

  xi64 = _mm256_mask_expand_epi64 (xi64, 0, xi64);
  xi64 = _mm256_mask_expand_epi64 (xi64, -1, xi64);
  xi64 = _mm256_mask_expand_epi64 (xi64, (1 << 3) - 1, xi64);
  xi64 = _mm256_mask_expand_epi64 (xi64, (1 << 3) + 2, xi64);

  xi64 = _mm256_mask_expandloadu_epi64 (xi64, 0, pi64);
  xi64 = _mm256_mask_expandloadu_epi64 (xi64, (1 << 4) - 1, pi64);
  xi64 = _mm256_mask_expandloadu_epi64 (xi64, (1 << 2) - 1, pi64);
  xi64 = _mm256_mask_expandloadu_epi64 (xi64, (1 << 2), pi64);

  xf = _mm256_mask_expand_ps (xf, 0, xf);
  xf = _mm256_mask_expand_ps (xf, (1 << 8) - 1, xf);
  xf = _mm256_mask_expand_ps (xf, (1 << 6) - 1, xf);
  xf = _mm256_mask_expand_ps (xf, (1 << 6) + 3, xf);

  xf = _mm256_mask_expandloadu_ps (xf, 0, pf);
  xf = _mm256_mask_expandloadu_ps (xf, -1, pf);
  xf = _mm256_mask_expandloadu_ps (xf, (1 << 7) - 1, pf);
  xf = _mm256_mask_expandloadu_ps (xf, (1 << 7) + 5, pf);

  xd = _mm256_mask_expand_pd (xd, 0, xd);
  xd = _mm256_mask_expand_pd (xd, (1 << 4) - 1, xd);
  xd = _mm256_mask_expand_pd (xd, (1 << 2) - 1, xd);
  xd = _mm256_mask_expand_pd (xd, (1 << 2), xd);

  xd = _mm256_mask_expandloadu_pd (xd, 0, pd);
  xd = _mm256_mask_expandloadu_pd (xd, -1, pd);
  xd = _mm256_mask_expandloadu_pd (xd, (1 << 2) - 1, pd);
  xd = _mm256_mask_expandloadu_pd (xd, (1 << 2), pd);

  xi32_128 = _mm_mask_expand_epi32 (xi32_128, 0, xi32_128);
  xi32_128 = _mm_mask_expand_epi32 (xi32_128, -1, xi32_128);
  xi32_128 = _mm_mask_expand_epi32 (xi32_128, (1 << 3) - 1, xi32_128);
  xi32_128 = _mm_mask_expand_epi32 (xi32_128, (1 << 3) + 1, xi32_128);

  xi32_128 = _mm_mask_expandloadu_epi32 (xi32_128, 0, pi32);
  xi32_128 = _mm_mask_expandloadu_epi32 (xi32_128, (1 << 4) - 1, pi32);
  xi32_128 = _mm_mask_expandloadu_epi32 (xi32_128, (1 << 2) - 1, pi32);
  xi32_128 = _mm_mask_expandloadu_epi32 (xi32_128, (1 << 1) + 3, pi32);

  xi64_128 = _mm_mask_expand_epi64 (xi64_128, 0, xi64_128);
  xi64_128 = _mm_mask_expand_epi64 (xi64_128, -1, xi64_128);
  xi64_128 = _mm_mask_expand_epi64 (xi64_128, (1 << 1) - 1, xi64_128);
  xi64_128 = _mm_mask_expand_epi64 (xi64_128, 2, xi64_128);

  xi64_128 = _mm_mask_expandloadu_epi64 (xi64_128, 0, pi64);
  xi64_128 = _mm_mask_expandloadu_epi64 (xi64_128, 3, pi64);
  xi64_128 = _mm_mask_expandloadu_epi64 (xi64_128, 1, pi64);
  xi64_128 = _mm_mask_expandloadu_epi64 (xi64_128, 2, pi64);

  xf_128 = _mm_mask_expand_ps (xf_128, 0, xf_128);
  xf_128 = _mm_mask_expand_ps (xf_128, (1 << 4) - 1, xf_128);
  xf_128 = _mm_mask_expand_ps (xf_128, (1 << 3) - 1, xf_128);
  xf_128 = _mm_mask_expand_ps (xf_128, (1 << 2), xf_128);

  xf_128 = _mm_mask_expandloadu_ps (xf_128, 0, pf);
  xf_128 = _mm_mask_expandloadu_ps (xf_128, -1, pf);
  xf_128 = _mm_mask_expandloadu_ps (xf_128, (1 << 3) - 1, pf);
  xf_128 = _mm_mask_expandloadu_ps (xf_128, (1 << 1), pf);

  xd_128 = _mm_mask_expand_pd (xd_128, 0, xd_128);
  xd_128 = _mm_mask_expand_pd (xd_128, (1 << 2) - 1, xd_128);
  xd_128 = _mm_mask_expand_pd (xd_128, 1, xd_128);
  xd_128 = _mm_mask_expand_pd (xd_128, 2, xd_128);

  xd_128 = _mm_mask_expandloadu_pd (xd_128, 0, pd);
  xd_128 = _mm_mask_expandloadu_pd (xd_128, -1, pd);
  xd_128 = _mm_mask_expandloadu_pd (xd_128, 1, pd);
  xd_128 = _mm_mask_expandloadu_pd (xd_128, 2, pd);
}
