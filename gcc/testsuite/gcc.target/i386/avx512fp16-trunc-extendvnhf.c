/* { dg-do compile } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -ftree-slp-vectorize -mprefer-vector-width=512" } */

extern double df[8];
extern float sf[16];
extern _Float16 hf[32];

#define DO_PRAGMA(X) _Pragma(#X)

#define TRUNCHFVV(size, mode)	    \
  void __attribute__ ((noinline, noclone))  \
truncv##size##mode##v##size##hf ()   \
{\
  int i;  \
  DO_PRAGMA (GCC unroll size)	\
  for (i = 0; i < size; i++)  \
    hf[i] = mode[i];  \
}

#define EXTENDHFVV(size, mode)	    \
  void __attribute__ ((noinline, noclone))  \
extendv##size##hf##v##size##mode ()   \
{\
  int i;  \
  DO_PRAGMA (GCC unroll size)	\
  for (i = 0; i < size; i++)  \
    mode[i] = hf[i];  \
}

TRUNCHFVV(8, df)
TRUNCHFVV(4, df)
TRUNCHFVV(2, df)
TRUNCHFVV(16, sf)
TRUNCHFVV(8, sf)
TRUNCHFVV(4, sf)
EXTENDHFVV(8, df)
EXTENDHFVV(4, df)
EXTENDHFVV(2, df)
EXTENDHFVV(16, sf)
EXTENDHFVV(8, sf)
EXTENDHFVV(4, sf)

/* { dg-final { scan-assembler-times "vcvtpd2phz\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phy\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtpd2phx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2phx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2phxy\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtps2phxx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2pd\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2pd\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2pd\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vcvtph2psx\[ \\t\]+\[^\{\n\]*\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

