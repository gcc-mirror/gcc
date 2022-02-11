/* PR target/103750 */
/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512bw -mavx512vl" } */
/* { dg-final { scan-assembler-not "kmov" { xfail ia32 } } } */
/* xfail need to be fixed.  */

#include <immintrin.h>
extern __m128i* pi128;
extern __m256i* pi256;
extern __m512i* pi512;

unsigned char
foo ()
{
  __mmask16 mask1 = _mm_cmpeq_epu8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epu8_mask (pi128[2], pi128[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo1 ()
{
  __mmask16 mask1 = _mm_cmpeq_epu8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epu8_mask (pi128[2], pi128[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo2 ()
{
  __mmask32 mask1 = _mm256_cmpeq_epu8_mask (pi256[0], pi256[1]);
  __mmask32 mask2 = _mm256_cmpeq_epu8_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo3 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu16_mask (pi128[1], pi128[2]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
foo4 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu16_mask (pi128[1], pi128[2]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo5 ()
{
  __mmask8 mask1 = _mm_cmpeq_epu16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epu16_mask (pi128[1], pi128[2]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo6 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epu16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epu16_mask (pi256[2], pi256[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
foo7 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epu16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epu16_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
foo8 ()
{
  __mmask32 mask1 = _mm512_cmpeq_epu16_mask (pi512[0], pi512[1]);
  __mmask32 mask2 = _mm512_cmpeq_epu16_mask (pi512[2], pi512[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo ()
{
  __mmask16 mask1 = _mm_cmpeq_epi8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epi8_mask (pi128[2], pi128[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo1 ()
{
  __mmask16 mask1 = _mm_cmpeq_epi8_mask (pi128[0], pi128[1]);
  __mmask16 mask2 = _mm_cmpeq_epi8_mask (pi128[2], pi128[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo2 ()
{
  __mmask32 mask1 = _mm256_cmpeq_epi8_mask (pi256[0], pi256[1]);
  __mmask32 mask2 = _mm256_cmpeq_epi8_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo3 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi16_mask (pi128[1], pi128[2]);
  return _kortestz_mask16_u8 (mask1, mask2);
}

unsigned char
sign_foo4 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi16_mask (pi128[1], pi128[2]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo5 ()
{
  __mmask8 mask1 = _mm_cmpeq_epi16_mask (pi128[0], pi128[1]);
  __mmask8 mask2 = _mm_cmpeq_epi16_mask (pi128[1], pi128[2]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo6 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epi16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epi16_mask (pi256[2], pi256[3]);
  return _kortestz_mask32_u8 (mask1, mask2);
}

unsigned char
sign_foo7 ()
{
  __mmask16 mask1 = _mm256_cmpeq_epi16_mask (pi256[0], pi256[1]);
  __mmask16 mask2 = _mm256_cmpeq_epi16_mask (pi256[2], pi256[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}

unsigned char
sign_foo8 ()
{
  __mmask32 mask1 = _mm512_cmpeq_epi16_mask (pi512[0], pi512[1]);
  __mmask32 mask2 = _mm512_cmpeq_epi16_mask (pi512[2], pi512[3]);
  return _kortestz_mask64_u8 (mask1, mask2);
}
