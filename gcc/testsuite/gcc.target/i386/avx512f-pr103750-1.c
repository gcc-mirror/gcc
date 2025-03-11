/* PR target/103750 */
/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512f -mavx512vl -mavx512bw" } */
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

unsigned char
foo ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu32_mask (pi128[2], pi128[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
foo1 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu32_mask (pi128[2], pi128[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo2 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu32_mask (pi128[2], pi128[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo3 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu32_mask (pi256[2], pi256[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
foo4 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu32_mask (pi256[2], pi256[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo5 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu32_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo6 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epu32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epu32_mask (pi512[2], pi512[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo7 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epu32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epu32_mask (pi512[2], pi512[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo8 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu64_mask (pi128[2], pi128[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
foo9 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu64_mask (pi128[2], pi128[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo10 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu64_mask (pi128[2], pi128[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo11 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu64_mask (pi256[2], pi256[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
foo12 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu64_mask (pi256[2], pi256[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo13 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epu64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epu64_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo14 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epu64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epu64_mask (pi512[2], pi512[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
foo15 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epu64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epu64_mask (pi512[2], pi512[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo16 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epu64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epu64_mask (pi512[2], pi512[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi32_mask (pi128[2], pi128[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
sign_foo1 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi32_mask (pi128[2], pi128[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo2 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi32_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi32_mask (pi128[2], pi128[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo3 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi32_mask (pi256[2], pi256[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
sign_foo4 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi32_mask (pi256[2], pi256[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo5 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi32_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi32_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo6 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epi32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epi32_mask (pi512[2], pi512[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo7 ()
{
  __mmask16 mask1 = _mm512_cmpeq_epi32_mask (pi512[0], pi512[1]);
  __mmask16 mask2 = _mm512_cmpeq_epi32_mask (pi512[2], pi512[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo8 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi64_mask (pi128[2], pi128[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
sign_foo9 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi64_mask (pi128[2], pi128[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo10 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi64_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi64_mask (pi128[2], pi128[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo11 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi64_mask (pi256[2], pi256[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
sign_foo12 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi64_mask (pi256[2], pi256[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo13 ()
{
  __mmask8 mask1 = _mm256_cmpeq_epi64_mask (pi256[0], pi256[1]);
  __mmask8 mask2 = _mm256_cmpeq_epi64_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo14 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epi64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epi64_mask (pi512[2], pi512[3]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
sign_foo15 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epi64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epi64_mask (pi512[2], pi512[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo16 ()
{
  __mmask8 mask1 = _mm512_cmpeq_epi64_mask (pi512[0], pi512[1]);
  __mmask8 mask2 = _mm512_cmpeq_epi64_mask (pi512[2], pi512[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
float_foo ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  __mmask8 mask2 = _mm_cmp_ps_mask (ps128[2], ps128[3], 1);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
float_foo1 ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  __mmask8 mask2 = _mm_cmp_ps_mask (ps128[2], ps128[3], 1);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
float_foo2 ()
{
  __mmask8 mask1 = _mm_cmp_ps_mask (ps128[0], ps128[1], 1);
  __mmask8 mask2 = _mm_cmp_ps_mask (ps128[2], ps128[3], 1);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
float_foo3 ()
{
  __mmask8 mask1 = _mm256_cmp_ps_mask (ps256[0], ps256[1], 1);
  __mmask8 mask2 = _mm256_cmp_ps_mask (ps256[2], ps256[3], 1);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
float_foo4 ()
{
  __mmask8 mask1 = _mm256_cmp_ps_mask (ps256[0], ps256[1], 1);
  __mmask8 mask2 = _mm256_cmp_ps_mask (ps256[2], ps256[3], 1);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
float_foo5 ()
{
  __mmask8 mask1 = _mm256_cmp_ps_mask (ps256[0], ps256[1], 1);
  __mmask8 mask2 = _mm256_cmp_ps_mask (ps256[2], ps256[3], 1);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
float_foo6 ()
{
  __mmask16 mask1 = _mm512_cmp_ps_mask (ps512[0], ps512[1], 1);
  __mmask16 mask2 = _mm512_cmp_ps_mask (ps512[2], ps512[3], 1);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
float_foo7 ()
{
  __mmask16 mask1 = _mm512_cmp_ps_mask (ps512[0], ps512[1], 1);
  __mmask16 mask2 = _mm512_cmp_ps_mask (ps512[2], ps512[3], 1);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
float_foo8 ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  __mmask8 mask2 = _mm_cmp_pd_mask (pd128[2], pd128[3], 1);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
float_foo9 ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  __mmask8 mask2 = _mm_cmp_pd_mask (pd128[2], pd128[3], 1);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
float_foo10 ()
{
  __mmask8 mask1 = _mm_cmp_pd_mask (pd128[0], pd128[1], 1);
  __mmask8 mask2 = _mm_cmp_pd_mask (pd128[2], pd128[3], 1);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
float_foo11 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  __mmask8 mask2 = _mm256_cmp_pd_mask (pd256[2], pd256[3], 1);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
float_foo12 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  __mmask8 mask2 = _mm256_cmp_pd_mask (pd256[2], pd256[3], 1);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
float_foo13 ()
{
  __mmask8 mask1 = _mm256_cmp_pd_mask (pd256[0], pd256[1], 1);
  __mmask8 mask2 = _mm256_cmp_pd_mask (pd256[2], pd256[3], 1);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
float_foo14 ()
{
  __mmask8 mask1 = _mm512_cmp_pd_mask (pd512[0], pd512[1], 1);
  __mmask8 mask2 = _mm512_cmp_pd_mask (pd512[2], pd512[3], 1);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
float_foo15 ()
{
  __mmask8 mask1 = _mm512_cmp_pd_mask (pd512[0], pd512[1], 1);
  __mmask8 mask2 = _mm512_cmp_pd_mask (pd512[2], pd512[3], 1);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
float_foo16 ()
{
  __mmask8 mask1 = _mm512_cmp_pd_mask (pd512[0], pd512[1], 1);
  __mmask8 mask2 = _mm512_cmp_pd_mask (pd512[2], pd512[3], 1);
  return _kortestz_mask64_u8 (mask1, mask2);
}
