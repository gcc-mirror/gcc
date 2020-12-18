/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bw -mavx512dq -O2" } */
/* { dg-final { scan-assembler-not "%k\[0-7\]" } } */

#include<immintrin.h>

#define FOO(VTYPE,PREFIX,SUFFIX,OPNAME,MASK,LEN)			\
  VTYPE								\
  foo_##LEN##_##SUFFIX##_##OPNAME (VTYPE a, VTYPE b)		\
  {									\
    MASK m = _mm##PREFIX##_cmp##OPNAME##_##SUFFIX##_mask (a, b);	\
    return _mm##PREFIX##_movm_##SUFFIX (m);				\
  }									\

FOO (__m128i,, epi8, eq, __mmask16, 128);
FOO (__m128i,, epi16, eq, __mmask8, 128);
FOO (__m128i,, epi32, eq, __mmask8, 128);
FOO (__m128i,, epi64, eq, __mmask8, 128);
FOO (__m128i,, epi8, gt, __mmask16, 128);
FOO (__m128i,, epi16, gt, __mmask8, 128);
FOO (__m128i,, epi32, gt, __mmask8, 128);
FOO (__m128i,, epi64, gt, __mmask8, 128);
FOO (__m256i, 256, epi8, eq, __mmask32, 256);
FOO (__m256i, 256, epi16, eq, __mmask16, 256);
FOO (__m256i, 256, epi32, eq, __mmask8, 256);
FOO (__m256i, 256, epi64, eq, __mmask8, 256);
FOO (__m256i, 256, epi8, gt, __mmask32, 256);
FOO (__m256i, 256, epi16, gt, __mmask16, 256);
FOO (__m256i, 256, epi32, gt, __mmask8, 256);
FOO (__m256i, 256, epi64, gt, __mmask8, 256);
