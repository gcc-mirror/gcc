/* { dg-do compile } */
/* { dg-require-effective-target avx } */
/* { dg-options "-O2 -mavx" } */

typedef int __v8si __attribute__((__vector_size__(32)));
typedef long long __m256i __attribute__((__vector_size__(32), __may_alias__));

static __m256i
_mm256_set1_epi32 (int __A)
{
  return __extension__ (__m256i)(__v8si){ __A, __A, __A, __A,
					  __A, __A, __A, __A };
}
__m256i
foo ()
{
  return _mm256_set1_epi32 (-1);
}
