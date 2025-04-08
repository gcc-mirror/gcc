/* PR target/103750 */
/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512vl" } */
/* { dg-final { scan-assembler-not "and" } } */

#include <immintrin.h>
extern __m128i* pi128;
extern __m256i* pi256;

extern __m128* ps128;
extern __m256* ps256;

extern __m128d* pd128;
extern __m256d* pd256;

extern char a;
void
foo ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  a = mask1 & 15;
}

void
foo1 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  a = mask1 & 3;
}

void
foo2 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  a = mask1 & 15;
}

void
sign_foo ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  a = mask1 & 15;
}

void
sign_foo1 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  a = mask1 & 3;
}


void
sign_foo2 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  a = mask1 & 15;
}

void
float_foo ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  a = mask1 & 15;
}

void
double_foo ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  a = mask1 & 3;
}

void
double_foo2 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  a = mask1 & 15;
}
