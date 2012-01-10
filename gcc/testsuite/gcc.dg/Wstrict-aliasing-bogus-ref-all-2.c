/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
/* { dg-options "-O2 -Wall -mabi=altivec" { target { { powerpc*-*-linux* } && ilp32 } } } */

typedef long long __m128i __attribute__ ((__vector_size__ (16), __may_alias__));

extern __inline __m128i __attribute__((__gnu_inline__, __always_inline__, __artificial__))
_mm_load_si128 (__m128i const *__P)
{
  return *__P;
}

static const short __attribute__((__aligned__(16))) tbl[8] =
{ 1, 2, 3, 4, 5, 6, 7, 8};


__m128i get_vec(void)
{
  __m128i ret;

  ret = _mm_load_si128((__m128i *)tbl); /* { dg-bogus "type-punning" } */

  return ret;
}

/* Ignore a warning that is irrelevant to the purpose of this test.  */
/* { dg-prune-output ".*GCC vector returned by reference.*" } */
