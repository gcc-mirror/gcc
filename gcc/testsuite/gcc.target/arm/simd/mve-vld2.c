/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O3" } */

#include <stdint.h>

#define M00 100
#define M10 216
#define M01 1322
#define M11 13

#define N 128


/* Integer tests.  */
#define FUNC(SIGN, TYPE, BITS)						\
  void foo_##SIGN##BITS##x (TYPE##BITS##_t *__restrict__ pInput,	\
			    TYPE##BITS##_t *__restrict__ pOutput)	\
  {									\
    unsigned int i;							\
    TYPE##BITS##_t  a, b;						\
    									\
    for (i = 0; i < N / BITS; i++)					\
      {									\
	a = *pInput++;							\
	b = *pInput++;							\
									\
	*pOutput++ = M00 * a + M01 * b;					\
	*pOutput++ = M10 * a + M11 * b;					\
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
#define FUNC_FLOAT(SIGN, TYPE, BITS)					\
  void foo_##SIGN##BITS##x (TYPE *__restrict__ pInput,			\
			    TYPE *__restrict__ pOutput)			\
  {									\
    unsigned int i;							\
    TYPE a, b;								\
    									\
    for (i = 0; i < N / BITS; i++)					\
      {									\
	a = *pInput++;							\
	b = *pInput++;							\
									\
	*pOutput++ = M00 * a + M01 * b;					\
	*pOutput++ = M10 * a + M11 * b;					\
      }									\
  }

FUNC_FLOAT(f, float, 32)

/* __fp16 test, needs explicit casts to avoid conversions to floating-point and
   failure to vectorize.  */
__fp16 M00_fp16 = 100.0f16;
__fp16 M10_fp16 = 216.0f16;
__fp16 M01_fp16 = 1322.0f16;
__fp16 M11_fp16 = 13.0f16;

#define FUNC_FLOAT_FP16(SIGN, TYPE, BITS)				\
  void foo_##SIGN##BITS##x (TYPE *__restrict__ pInput,			\
			    TYPE *__restrict__ pOutput)			\
  {									\
    unsigned int i;							\
    TYPE a, b;								\
    									\
    for (i = 0; i < N / BITS; i++)					\
      {									\
	a = *pInput++;							\
	b = *pInput++;							\
									\
	*pOutput++ = (__fp16)(M00_fp16 * a) + (__fp16)(M01_fp16 * b);	\
	*pOutput++ = (__fp16)(M10_fp16 * a) + (__fp16)(M11_fp16 * b);	\
      }									\
  }

FUNC_FLOAT_FP16(f, __fp16, 16)

/* vld2X.8 is used for signed and unsigned chars: 2 pairs.  */
/* vld2X.16 is used for signed and unsigned shorts and __fp16: 3 pairs.  */
/* vld2X.32 is used for signed and unsigned ints and float: 3 pairs.  */
/* { dg-final { scan-assembler-times {vld2[01].8\t.q[0-9]+, q[0-9]+., } 4 } } */
/* { dg-final { scan-assembler-times {vld2[01].16\t.q[0-9]+, q[0-9]+., } 6 } } */
/* { dg-final { scan-assembler-times {vld2[01].32\t.q[0-9]+, q[0-9]+., } 6 } } */
/* { dg-final { scan-assembler-times {vst2[01].8\t.q[0-9]+, q[0-9]+., } 4 } } */
/* { dg-final { scan-assembler-times {vst2[01].16\t.q[0-9]+, q[0-9]+., } 6 } } */
/* { dg-final { scan-assembler-times {vst2[01].32\t.q[0-9]+, q[0-9]+., } 6 } } */
