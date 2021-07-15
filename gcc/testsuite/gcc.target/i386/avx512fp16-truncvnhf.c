/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-slp-vectorize -mprefer-vector-width=512" } */

extern long long di[8];
extern unsigned long long udi[8];
extern int si[16];
extern unsigned int usi[16];
extern short hi[32];
extern unsigned short uhi[32];
extern _Float16 hf[32];

#define DO_PRAGMA(X) _Pragma(#X)

#define FIX_TRUNCHFVV(size, mode)	    \
  void __attribute__ ((noinline, noclone))  \
fix_trunc##size##hf##v##size##mode ()   \
{\
  int i;  \
  DO_PRAGMA (GCC unroll size)	\
  for (i = 0; i < size; i++)  \
    mode[i] = hf[i];  \
}

FIX_TRUNCHFVV(32, hi)
FIX_TRUNCHFVV(16, hi)
FIX_TRUNCHFVV(8, hi)
FIX_TRUNCHFVV(16, si)
FIX_TRUNCHFVV(8, si)
FIX_TRUNCHFVV(4, si)
FIX_TRUNCHFVV(8, di)
FIX_TRUNCHFVV(4, di)
FIX_TRUNCHFVV(2, di)

FIX_TRUNCHFVV(32, uhi)
FIX_TRUNCHFVV(16, uhi)
FIX_TRUNCHFVV(8, uhi)
FIX_TRUNCHFVV(16, usi)
FIX_TRUNCHFVV(8, usi)
FIX_TRUNCHFVV(4, usi)
FIX_TRUNCHFVV(8, udi)
FIX_TRUNCHFVV(4, udi)
FIX_TRUNCHFVV(2, udi)

/* { dg-final { scan-assembler-times "vcvttph2qq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uqq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2qq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uqq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2qq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uqq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2dq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2dq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2dq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2udq\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2w\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2w\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2w\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvttph2uw\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
