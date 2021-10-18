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

#define FLOATHFVV(size, mode)	    \
  void __attribute__ ((noinline, noclone))  \
float##v##size##mode##v##size##hf ()   \
{\
  int i;  \
  DO_PRAGMA (GCC unroll size)	\
  for (i = 0; i < size; i++)  \
    hf[i] = (_Float16) mode[i];  \
}

FLOATHFVV(32, hi)
FLOATHFVV(16, hi)
FLOATHFVV(8, hi)
FLOATHFVV(16, si)
FLOATHFVV(8, si)
FLOATHFVV(4, si)
FLOATHFVV(8, di)
FLOATHFVV(4, di)
FLOATHFVV(2, di)

FLOATHFVV(32, uhi)
FLOATHFVV(16, uhi)
FLOATHFVV(8, uhi)
FLOATHFVV(16, usi)
FLOATHFVV(8, usi)
FLOATHFVV(4, usi)
FLOATHFVV(8, udi)
FLOATHFVV(4, udi)
FLOATHFVV(2, udi)

/* { dg-final { scan-assembler-times "vcvtqq2phz\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2phz\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtqq2phy\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2phy\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtqq2phx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuqq2phx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtdq2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtudq2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtdq2phy\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtudq2phy\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtdq2phx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtudq2phx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtw2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtuw2ph\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
