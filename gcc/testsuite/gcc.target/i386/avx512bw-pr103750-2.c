/* PR target/103750 */
/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512dq -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-not "kmov" } } */

#include <immintrin.h>
extern __m128i* pi128;
extern __m256i* pi256;
extern __m512i* pi512;

extern char a, b;
void
foo ()
{
  __mmask16 mask1 = _mm_cmpeq_epu8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epu8_mask (pi128[2], pi128[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
foo1 ()
{
  __mmask16 mask1 = _mm_cmpeq_epu8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epu8_mask (pi128[2], pi128[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
foo2 ()
{
  __mmask32 mask1 = _mm256_cmpeq_epu8_mask (pi256[0], pi256[1]);
  __mmask32 mask2 = _mm256_cmpeq_epu8_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask32_u8 (mask1, mask2);
}

void
foo3 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu16_mask (pi128[1], pi128[2]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo4 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu16_mask (pi128[1], pi128[2]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo5 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu16_mask (pi128[1], pi128[2]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
foo6 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epu16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epu16_mask (pi256[2], pi256[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
foo7 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epu16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epu16_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
foo8 ()
{
  __mmask32 mask1 = _mm512_cmpeq_epu16_mask (pi512[0], pi512[1]);
  __mmask32 mask2 = _mm512_cmpeq_epu16_mask (pi512[2], pi512[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask32_u8 (mask1, mask2);
}

void
sign_foo ()
{
  __mmask16 mask1 = _mm_cmpeq_epi8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epi8_mask (pi128[2], pi128[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo1 ()
{
  __mmask16 mask1 = _mm_cmpeq_epi8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epi8_mask (pi128[2], pi128[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo2 ()
{
  __mmask32 mask1 = _mm256_cmpeq_epi8_mask (pi256[0], pi256[1]);
  __mmask32 mask2 = _mm256_cmpeq_epi8_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask32_u8 (mask1, mask2);
}

void
sign_foo3 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi16_mask (pi128[1], pi128[2]);
  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo4 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi16_mask (pi128[1], pi128[2]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo5 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi16_mask (pi128[1], pi128[2]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo6 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epi16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epi16_mask (pi256[2], pi256[3]);
  a = _kortestz_mask32_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo7 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epi16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epi16_mask (pi256[2], pi256[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo8 ()
{
  __mmask32 mask1 = _mm512_cmpeq_epi16_mask (pi512[0], pi512[1]);
  __mmask32 mask2 = _mm512_cmpeq_epi16_mask (pi512[2], pi512[3]);
  a = _kortestz_mask64_u8 (mask1, mask2);
  b = _kortestz_mask32_u8 (mask1, mask2);
}
