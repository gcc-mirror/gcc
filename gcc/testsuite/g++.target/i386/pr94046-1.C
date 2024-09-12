// PR target/94046
// { dg-do compile }
// { dg-options "-O2 -mavx2 -mxop" }
// { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } }

#include <x86intrin.h>

#define S(x) struct x { operator __##x (); };
S (m128)
S (m128d)
S (m128i)
S (m256)
S (m256d)
S (m256i)

__m128
f1 (m128 src, float const *base, m128i idx, m128 mask)
{
  return _mm_mask_i32gather_ps (src, base, idx, mask, 2);
}

__m256
f2 (m256 src, float const *base, m256i idx, m256 mask)
{
  return _mm256_mask_i32gather_ps (src, base, idx, mask, 2);
}

__m128
f3 (m128 src, float const *base, m128i idx, m128 mask)
{
  return _mm_mask_i64gather_ps (src, base, idx, mask, 2);
}

__m128d
f4 (m128d x, m128d y, m128i c)
{
  return _mm_permute2_pd (x, y, c, 3);
}

__m128
f5 (m128 x, m128 y, m128i c)
{
  return _mm_permute2_ps (x, y, c, 3);
}

__m256d
f6 (m256d x, m256d y, m256i c)
{
  return _mm256_permute2_pd (x, y, c, 3);
}

__m256
f7 (m256 x, m256 y, m256i c)
{
  return _mm256_permute2_ps (x, y, c, 3);
}
