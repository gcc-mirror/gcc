/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define M00 100
#define M10 216
#define M20 23
#define M30 237
#define M01 1322
#define M11 13
#define M21 27271
#define M31 2280
#define M02 74
#define M12 191
#define M22 500
#define M32 111
#define M03 134
#define M13 117
#define M23 11
#define M33 771

#define N 128

/* Integer tests.  */
#define FUNC(SIGN, TYPE, BITS)						\
  void foo_##SIGN##BITS##x (TYPE##BITS##_t *__restrict__ pInput,	\
			    TYPE##BITS##_t *__restrict__ pOutput)	\
  {									\
    unsigned int i;							\
    TYPE##BITS##_t  a, b, c, d;						\
    									\
    for (i = 0; i < N / BITS; i++)					\
      {									\
	a = *pInput++;							\
	b = *pInput++;							\
	c = *pInput++;							\
	d = *pInput++;							\
									\
	*pOutput++ = M00 * a + M01 * b + M02 * c + M03 * d;		\
	*pOutput++ = M10 * a + M11 * b + M12 * c + M13 * d;		\
	*pOutput++ = M20 * a + M21 * b + M22 * c + M23 * d;		\
	*pOutput++ = M30 * a + M31 * b + M32 * c + M33 * d;		\
      }									\
  }

FUNC(s, int, 8)
FUNC(u, uint, 8)
FUNC(s, int, 16)
FUNC(u, uint, 16)
FUNC(s, int, 32)
FUNC(u, uint, 32)

/* float test, keep the macro because it's similar to the above, but does not
   need the ##BITS##_t.  */
#define FUNC_FLOAT(SIGN, TYPE, BITS)						\
  void foo_##SIGN##BITS##x (TYPE *__restrict__ pInput,			\
			    TYPE *__restrict__ pOutput)			\
  {									\
    unsigned int i;							\
    TYPE a, b, c, d;							\
    									\
    for (i = 0; i < N / BITS; i++)					\
      {									\
	a = *pInput++;							\
	b = *pInput++;							\
	c = *pInput++;							\
	d = *pInput++;							\
									\
	*pOutput++ = M00 * a + M01 * b + M02 * c + M03 * d;		\
	*pOutput++ = M10 * a + M11 * b + M12 * c + M13 * d;		\
	*pOutput++ = M20 * a + M21 * b + M22 * c + M23 * d;		\
	*pOutput++ = M30 * a + M31 * b + M32 * c + M33 * d;		\
      }									\
  }

FUNC_FLOAT(f, float, 32)

/* __fp16 test, needs explicit casts to avoid conversions to floating-point and
   failure to vectorize.  */
__fp16 M00_fp16 = 100.0f16;
__fp16 M10_fp16 = 216.0f16;
__fp16 M20_fp16 = 23.0f16;
__fp16 M30_fp16 = 237.0f16;
__fp16 M01_fp16 = 1322.0f16;
__fp16 M11_fp16 = 13.0f16;
__fp16 M21_fp16 = 27271.0f16;
__fp16 M31_fp16 = 2280.0f16;
__fp16 M02_fp16 = 74.0f16;
__fp16 M12_fp16 = 191.0f16;
__fp16 M22_fp16 = 500.0f16;
__fp16 M32_fp16 = 111.0f16;
__fp16 M03_fp16 = 134.0f16;
__fp16 M13_fp16 = 117.0f16;
__fp16 M23_fp16 = 11.0f16;
__fp16 M33_fp16 = 771.0f16;

#define FUNC_FLOAT_FP16(SIGN, TYPE, BITS)				\
  void foo_##SIGN##BITS##x (TYPE *__restrict__ pInput,			\
			    TYPE *__restrict__ pOutput)			\
  {									\
    unsigned int i;							\
    TYPE a, b, c, d;							\
    									\
    for (i = 0; i < N / BITS; i++)					\
      {									\
	a = *pInput++;							\
	b = *pInput++;							\
	c = *pInput++;							\
	d = *pInput++;							\
									\
	TYPE ab, cd;							\
	ab = (__fp16)(M00_fp16 * a) + (__fp16)(M01_fp16 * b);		\
	cd = (__fp16)(M02_fp16 * c) + (__fp16)(M03_fp16 * d);		\
	*pOutput++ = ab + cd;						\
	ab = (__fp16)(M10_fp16 * a) + (__fp16)(M11_fp16 * b);		\
	cd = (__fp16)(M12_fp16 * c) + (__fp16)(M13_fp16 * d);		\
	*pOutput++ = ab + cd;						\
	ab = (__fp16)(M20_fp16 * a) + (__fp16)(M21_fp16 * b);		\
	cd = (__fp16)(M22_fp16 * c) + (__fp16)(M23_fp16 * d);		\
	*pOutput++ = ab + cd;						\
	ab = (__fp16)(M30_fp16 * a) + (__fp16)(M31_fp16 * b);		\
	cd = (__fp16)(M32_fp16 * c) + (__fp16)(M33_fp16 * d);		\
	*pOutput++ = ab + cd;						\
      }									\
  }

FUNC_FLOAT_FP16(f, __fp16, 16)

/* vld4X.8 is used for signed and unsigned chars: 2 * 4.  */
/* vld4X.16 is used for signed and unsigned shorts and __fp16: 3 * 4.  */
/* vld4X.32 is used for signed and unsigned ints and float: 3 * 4.  */
/* { dg-final { scan-assembler-times {vld4[0123].8\t.q[0-9]+, q[0-9]+, q[0-9]+, q[0-9]+., } 8 } } */
/* { dg-final { scan-assembler-times {vld4[0123].16\t.q[0-9]+, q[0-9]+, q[0-9]+, q[0-9]+., } 12 } } */
/* { dg-final { scan-assembler-times {vld4[0123].32\t.q[0-9]+, q[0-9]+, q[0-9]+, q[0-9]+., } 12 } } */
/* { dg-final { scan-assembler-times {vst4[0123].8\t.q[0-9]+, q[0-9]+, q[0-9]+, q[0-9]+., } 8 } } */
/* { dg-final { scan-assembler-times {vst4[0123].16\t.q[0-9]+, q[0-9]+, q[0-9]+, q[0-9]+., } 12 } } */
/* { dg-final { scan-assembler-times {vst4[0123].32\t.q[0-9]+, q[0-9]+, q[0-9]+, q[0-9]+., } 12 } } */
