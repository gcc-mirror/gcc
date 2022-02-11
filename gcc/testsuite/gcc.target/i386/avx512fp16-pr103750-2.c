/* PR target/103750 */
/* { dg-do compile }  */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */
/* { dg-final { scan-assembler-not "kmov" { xfail ia32 } } } */
/* xfail need to be fixed.  */

#include <immintrin.h>
extern __m128h* ph128;
extern __m256h* ph256;
extern __m512h* ph512;

extern char a, b;
void
sign_foo3 ()
{
  __mmask8 mask1 = _mm_cmp_ph_mask (ph128[0], ph128[1], 1);
  __mmask8 mask2 = _mm_cmp_ph_mask (ph128[1], ph128[2], 1);

  a = _kortestz_mask16_u8 (mask1, mask2);
  b = _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo4 ()
{
  __mmask8 mask1 = _mm_cmp_ph_mask (ph128[0], ph128[1], 1);
  __mmask8 mask2 = _mm_cmp_ph_mask (ph128[1], ph128[2], 1);

  a =  _kortestz_mask32_u8 (mask1, mask2);
  b =  _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo5 ()
{
  __mmask8 mask1 = _mm_cmp_ph_mask (ph128[0], ph128[1], 1);
  __mmask8 mask2 = _mm_cmp_ph_mask (ph128[1], ph128[2], 1);

  a =  _kortestz_mask64_u8 (mask1, mask2);
  b =  _kortestz_mask8_u8 (mask1, mask2);
}

void
sign_foo6 ()
{
  __mmask16 mask1 = _mm256_cmp_ph_mask (ph256[0], ph256[1], 1);
  __mmask16 mask2 = _mm256_cmp_ph_mask (ph256[2], ph256[3], 1);

  a =  _kortestz_mask32_u8 (mask1, mask2);
  b =  _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo7 ()
{
  __mmask16 mask1 = _mm256_cmp_ph_mask (ph256[0], ph256[1], 1);
  __mmask16 mask2 = _mm256_cmp_ph_mask (ph256[2], ph256[3], 1);

  a =  _kortestz_mask64_u8 (mask1, mask2);
  b =  _kortestz_mask16_u8 (mask1, mask2);
}

void
sign_foo8 ()
{
  __mmask32 mask1 = _mm512_cmp_ph_mask (ph512[0], ph512[1], 1);
  __mmask32 mask2 = _mm512_cmp_ph_mask (ph512[2], ph512[3], 1);

  a =  _kortestz_mask64_u8 (mask1, mask2);
  b =  _kortestz_mask32_u8 (mask1, mask2);
}
