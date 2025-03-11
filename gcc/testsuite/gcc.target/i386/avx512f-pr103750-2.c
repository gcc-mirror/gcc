/* PR target/103750 */
/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512dq -mavx512vl -mavx512bw" } */
/* { dg-final { scan-assembler-not "kmov" } } */

#include <immintrin.h>
extern __m128i* pi128;
extern __m256i* pi256;
extern __m512i* pi512;

extern __m128* ps128;
extern __m256* ps256;
extern __m512* ps512;

extern __m128d* pd128;
extern __m256d* pd256;
extern __m512d* pd512;

extern char a, b;
void
foo ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu32_mask (pi128[2], pi128[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo1 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu32_mask (pi128[2], pi128[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo2 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu32_mask (pi128[2], pi128[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo3 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu32_mask (pi256[2], pi256[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo4 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu32_mask (pi256[2], pi256[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo5 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu32_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo6 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epu32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epu32_mask (pi512[2], pi512[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
foo7 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epu32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epu32_mask (pi512[2], pi512[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
foo8 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu64_mask (pi128[2], pi128[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo9 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu64_mask (pi128[2], pi128[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo10 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu64_mask (pi128[2], pi128[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo11 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu64_mask (pi256[2], pi256[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo12 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu64_mask (pi256[2], pi256[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo13 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu64_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo14 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epu64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epu64_mask (pi512[2], pi512[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo15 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epu64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epu64_mask (pi512[2], pi512[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo16 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epu64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epu64_mask (pi512[2], pi512[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi32_mask (pi128[2], pi128[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo1 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi32_mask (pi128[2], pi128[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo2 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi32_mask (pi128[2], pi128[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo3 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi32_mask (pi256[2], pi256[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo4 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi32_mask (pi256[2], pi256[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo5 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi32_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo6 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epi32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epi32_mask (pi512[2], pi512[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo7 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epi32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epi32_mask (pi512[2], pi512[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo8 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi64_mask (pi128[2], pi128[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo9 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi64_mask (pi128[2], pi128[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo10 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi64_mask (pi128[2], pi128[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo11 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi64_mask (pi256[2], pi256[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo12 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi64_mask (pi256[2], pi256[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo13 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi64_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo14 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epi64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epi64_mask (pi512[2], pi512[3]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo15 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epi64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epi64_mask (pi512[2], pi512[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo16 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epi64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epi64_mask (pi512[2], pi512[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  __mmask8 mask2 = _mm_cmp_ps_mask (ps128[2], ps128[3], 1);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo1 ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  __mmask8 mask2 = _mm_cmp_ps_mask (ps128[2], ps128[3], 1);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo2 ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  __mmask8 mask2 = _mm_cmp_ps_mask (ps128[2], ps128[3], 1);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo3 ()
{
  __mmask8 mask1 = _mm256_cmp_ps_mask (ps256[0], ps256[1], 1);
  __mmask8 mask2 = _mm256_cmp_ps_mask (ps256[2], ps256[3], 1);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo4 ()
{
  __mmask8 mask1 = _mm256_cmp_ps_mask (ps256[0], ps256[1], 1);
  __mmask8 mask2 = _mm256_cmp_ps_mask (ps256[2], ps256[3], 1);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo5 ()
{
  __mmask8 mask1 = _mm256_cmp_ps_mask (ps256[0], ps256[1], 1);
  __mmask8 mask2 = _mm256_cmp_ps_mask (ps256[2], ps256[3], 1);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo6 ()
{
  __mmask16 mask1 = _mm512_cmp_ps_mask (ps512[0], ps512[1], 1);
  __mmask16 mask2 = _mm512_cmp_ps_mask (ps512[2], ps512[3], 1);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
float_foo7 ()
{
  __mmask16 mask1 = _mm512_cmp_ps_mask (ps512[0], ps512[1], 1);
  __mmask16 mask2 = _mm512_cmp_ps_mask (ps512[2], ps512[3], 1);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
float_foo8 ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  __mmask8 mask2 = _mm_cmp_pd_mask (pd128[2], pd128[3], 1);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo9 ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  __mmask8 mask2 = _mm_cmp_pd_mask (pd128[2], pd128[3], 1);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo10 ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  __mmask8 mask2 = _mm_cmp_pd_mask (pd128[2], pd128[3], 1);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo11 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  __mmask8 mask2 = _mm256_cmp_pd_mask (pd256[2], pd256[3], 1);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo12 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  __mmask8 mask2 = _mm256_cmp_pd_mask (pd256[2], pd256[3], 1);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo13 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  __mmask8 mask2 = _mm256_cmp_pd_mask (pd256[2], pd256[3], 1);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo14 ()
{
  __mmask8 mask1 = _mm512_cmp_pd_mask (pd512[0], pd512[1], 1);
  __mmask8 mask2 = _mm512_cmp_pd_mask (pd512[2], pd512[3], 1);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo15 ()
{
  __mmask8 mask1 = _mm512_cmp_pd_mask (pd512[0], pd512[1], 1);
  __mmask8 mask2 = _mm512_cmp_pd_mask (pd512[2], pd512[3], 1);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
float_foo16 ()
{
  __mmask8 mask1 = _mm512_cmp_pd_mask (pd512[0], pd512[1], 1);
  __mmask8 mask2 = _mm512_cmp_pd_mask (pd512[2], pd512[3], 1);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}
