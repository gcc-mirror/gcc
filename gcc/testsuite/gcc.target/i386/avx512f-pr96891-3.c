/* { dg-do compile } */
/* { dg-options "-mavx512vl -mavx512bw -mavx512dq -O2 -masm=att -mstv -mno-stackrealign" } */
/* { dg-final { scan-assembler-not {not[bwlqd]\]} } } */
/* { dg-final { scan-assembler-times {(?n)vpcmp[bwdq][ \t]*\$5} 4} } */
/* { dg-final { scan-assembler-times {(?n)vpcmp[bwdq][ \t]*\$6} 4} } */
/* { dg-final { scan-assembler-times {(?n)vpcmp[bwdq][ \t]*\$[37]} 4} } */
/* { dg-final { scan-assembler-times {(?n)vcmpp[sd][ \t]*\$5} 2} } */
/* { dg-final { scan-assembler-times {(?n)vcmpp[sd][ \t]*\$6} 2} } */
/* { dg-final { scan-assembler-times {(?n)vcmpp[sd][ \t]*\$7} 2} } */

#include<immintrin.h>

#define FOO(VTYPE,PREFIX,SUFFIX,MASK,LEN,CMPIMM)			\
  MASK								\
  foo_##LEN##_##SUFFIX##_##CMPIMM (VTYPE a, VTYPE b)				\
  {									\
    MASK m = _mm##PREFIX##_cmp_##SUFFIX##_mask (a, b, CMPIMM);		\
    return ~m;								\
  }									\

FOO (__m128i,, epi8, __mmask16, 128, 1);
FOO (__m128i,, epi16, __mmask8, 128, 1);
FOO (__m128i,, epi32, __mmask8, 128, 1);
FOO (__m128i,, epi64, __mmask8, 128, 1);
FOO (__m256i, 256, epi8, __mmask32, 256, 2);
FOO (__m256i, 256, epi16, __mmask16, 256, 2);
FOO (__m256i, 256, epi32, __mmask8, 256, 2);
FOO (__m256i, 256, epi64, __mmask8, 256, 2);
FOO (__m512i, 512, epi8, __mmask64, 512, 3);
FOO (__m512i, 512, epi16, __mmask32, 512, 3);
FOO (__m512i, 512, epi32, __mmask16, 512, 3);
FOO (__m512i, 512, epi64, __mmask8, 512, 3);

FOO (__m128,, ps, __mmask8, 128, 1);
FOO (__m128d,, pd, __mmask8, 128, 1);
FOO (__m256, 256, ps, __mmask8, 256, 2);
FOO (__m256d, 256, pd, __mmask8, 256, 2);
FOO (__m512, 512, ps, __mmask16, 512, 3);
FOO (__m512d, 512, pd, __mmask8, 512, 3);
