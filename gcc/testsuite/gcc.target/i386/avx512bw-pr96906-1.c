/* PR target/96906 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512bw -mavx512vl -masm=att" } */
/* { dg-final { scan-assembler-times {(?n)vpcmpub[ \t]*\$2} 9 } } */
/* { dg-final { scan-assembler-times {(?n)vpcmpub[ \t]*\$6} 9 } } */
/* { dg-final { scan-assembler-times {(?n)vpcmpuw[ \t]*\$2} 9 } } */
/* { dg-final { scan-assembler-times {(?n)vpcmpuw[ \t]*\$6} 9 } } */


#include<immintrin.h>

#define FOO(LENGTH,SUFFIX,TYPE,UTYPE,RTYPE,PRED)			\
  __mmask##RTYPE							\
  foo_##LENGTH##_##TYPE##_##PRED (__m##LENGTH##i x, __m##LENGTH##i y)	\
  {									\
    return								\
      _mm##SUFFIX##_cmp_##TYPE##_mask (_mm##SUFFIX##_subs_##UTYPE (x, y), \
				       _mm##SUFFIX##_setzero_si##LENGTH (), \
				       PRED);				\
  }									\

FOO (128,, epi16, epu16, 8, 0);
FOO (128,, epi16, epu16, 8, 4);

FOO (128,, epu16, epu16, 8, 0);
FOO (128,, epu16, epu16, 8, 2);
FOO (128,, epu16, epu16, 8, 4);
FOO (128,, epu16, epu16, 8, 6);

FOO (256, 256, epi16, epu16, 16, 0);
FOO (256, 256, epi16, epu16, 16, 4);

FOO (256, 256, epu16, epu16, 16, 0);
FOO (256, 256, epu16, epu16, 16, 2);
FOO (256, 256, epu16, epu16, 16, 4);
FOO (256, 256, epu16, epu16, 16, 6);

FOO (512, 512, epi16, epu16, 32, 0);
FOO (512, 512, epi16, epu16, 32, 4);

FOO (512, 512, epu16, epu16, 32, 0);
FOO (512, 512, epu16, epu16, 32, 2);
FOO (512, 512, epu16, epu16, 32, 4);
FOO (512, 512, epu16, epu16, 32, 6);

FOO (128,, epi8, epu8, 16, 0);
FOO (128,, epi8, epu8, 16, 4);

FOO (128,, epu8, epu8, 16, 0);
FOO (128,, epu8, epu8, 16, 2);
FOO (128,, epu8, epu8, 16, 4);
FOO (128,, epu8, epu8, 16, 6);

FOO (256, 256, epi8, epu8, 32, 0);
FOO (256, 256, epi8, epu8, 32, 4);

FOO (256, 256, epu8, epu8, 32, 0);
FOO (256, 256, epu8, epu8, 32, 2);
FOO (256, 256, epu8, epu8, 32, 4);
FOO (256, 256, epu8, epu8, 32, 6);

FOO (512, 512, epi8, epu8, 64, 0);
FOO (512, 512, epi8, epu8, 64, 4);

FOO (512, 512, epu8, epu8, 64, 0);
FOO (512, 512, epu8, epu8, 64, 2);
FOO (512, 512, epu8, epu8, 64, 4);
FOO (512, 512, epu8, epu8, 64, 6);
