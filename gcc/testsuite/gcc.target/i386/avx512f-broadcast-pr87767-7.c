/* PR target/87767 */
/* { dg-do compile } */
/* { dg-options "-O2 -mavx512f -mavx512vl" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { *-*-darwin* && ia32 } } }
/* { dg-final { scan-assembler-times "vadd\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vadd\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vadd\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vadd\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vsub\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vsub\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vsub\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vsub\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vmul\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vmul\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vmul\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vmul\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vdiv\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vdiv\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vdiv\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vdiv\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfmadd\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfmadd\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfmadd\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfmadd\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfmsub\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfmsub\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfmsub\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfmsub\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfnmadd\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfnmadd\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfnmadd\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfnmadd\[^\n\]*\\\{1to16\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfnmsub\[^\n\]*\\\{1to2\\\}" 1 } }  */
/* { dg-final { scan-assembler-times "vfnmsub\[^\n\]*\\\{1to4\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfnmsub\[^\n\]*\\\{1to8\\\}" 2 } }  */
/* { dg-final { scan-assembler-times "vfnmsub\[^\n\]*\\\{1to16\\\}" 1 } }  */

#include<immintrin.h>

#define CONSTANT 101

#define FOO(VTYPE, OP_NAME, LEN, SUFFIX, MTYPE)			\
  VTYPE									\
  __attribute__ ((noipa))						\
  _mm##LEN##_foo_##OP_NAME##_##SUFFIX (VTYPE dst, VTYPE src, MTYPE m)	\
  {									\
    return  _mm##LEN##_mask_##OP_NAME##_##SUFFIX (dst, m, src,		\
						  _mm##LEN##_set1_##SUFFIX (CONSTANT)); \
  }									\

#define FOOZ(VTYPE, OP_NAME, LEN, SUFFIX, MTYPE)			\
  VTYPE									\
  __attribute__ ((noipa))						\
  _mm##LEN##_fooz_##OP_NAME##_##SUFFIX (VTYPE dst, VTYPE src, MTYPE m)	\
  {									\
    return  _mm##LEN##_maskz_##OP_NAME##_##SUFFIX (m, dst, src,		\
						  _mm##LEN##_set1_##SUFFIX (CONSTANT)); \
  }									\

FOO (__m512, add, 512, ps, __mmask16);
FOO (__m256, add, 256, ps, __mmask8);
FOO (__m128, add,, ps, __mmask8);

FOO (__m512, sub, 512, ps, __mmask16);
FOO (__m256, sub, 256, ps, __mmask8);
FOO (__m128, sub,, ps, __mmask8);

FOO (__m512, mul, 512, ps, __mmask16);
FOO (__m256, mul, 256, ps, __mmask8);
FOO (__m128, mul,, ps, __mmask8);

FOO (__m512, div, 512, ps, __mmask16);
FOO (__m256, div, 256, ps, __mmask8);
FOO (__m128, div,, ps, __mmask8);

FOOZ (__m512, fmadd, 512, ps, __mmask16);
FOOZ (__m256, fmadd, 256, ps, __mmask8);
FOOZ (__m128, fmadd,, ps, __mmask8);

FOOZ (__m512, fmsub, 512, ps, __mmask16);
FOOZ (__m256, fmsub, 256, ps, __mmask8);
FOOZ (__m128, fmsub,, ps, __mmask8);

FOOZ (__m512, fnmadd, 512, ps, __mmask16);
FOOZ (__m256, fnmadd, 256, ps, __mmask8);
FOOZ (__m128, fnmadd,, ps, __mmask8);

FOOZ (__m512, fnmsub, 512, ps, __mmask16);
FOOZ (__m256, fnmsub, 256, ps, __mmask8);
FOOZ (__m128, fnmsub,, ps, __mmask8);

FOO (__m512d, add, 512, pd, __mmask8);
FOO (__m256d, add, 256, pd, __mmask8);
FOO (__m128d, add,, pd, __mmask8);

FOO (__m512d, sub, 512, pd, __mmask8);
FOO (__m256d, sub, 256, pd, __mmask8);
FOO (__m128d, sub,, pd, __mmask8);

FOO (__m512d, mul, 512, pd, __mmask8);
FOO (__m256d, mul, 256, pd, __mmask8);
FOO (__m128d, mul,, pd, __mmask8);

FOO (__m512d, div, 512, pd, __mmask8);
FOO (__m256d, div, 256, pd, __mmask8);
FOO (__m128d, div,, pd, __mmask8);

FOOZ (__m512d, fmadd, 512, pd, __mmask8);
FOOZ (__m256d, fmadd, 256, pd, __mmask8);
FOOZ (__m128d, fmadd,, pd, __mmask8);

FOOZ (__m512d, fmsub, 512, pd, __mmask8);
FOOZ (__m256d, fmsub, 256, pd, __mmask8);
FOOZ (__m128d, fmsub,, pd, __mmask8);

FOOZ (__m512d, fnmadd, 512, pd, __mmask8);
FOOZ (__m256d, fnmadd, 256, pd, __mmask8);
FOOZ (__m128d, fnmadd,, pd, __mmask8);

FOOZ (__m512d, fnmsub, 512, pd, __mmask8);
FOOZ (__m256d, fnmsub, 256, pd, __mmask8);
FOOZ (__m128d, fnmsub,, pd, __mmask8);
