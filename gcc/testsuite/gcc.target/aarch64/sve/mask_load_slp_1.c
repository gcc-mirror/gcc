/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

#define MASK_SLP_2(TYPE_COND, ALT_VAL)					\
void __attribute__ ((noinline, noclone))				\
mask_slp_##TYPE_COND##_2_##ALT_VAL (int *restrict x, int *restrict y,	\
				    TYPE_COND *restrict z, int n)	\
{									\
  for (int i = 0; i < n; i += 2)					\
    {									\
      x[i] = y[i] ? z[i] : 1;						\
      x[i + 1] = y[i + 1] ? z[i + 1] : ALT_VAL;				\
    }									\
}

#define MASK_SLP_4(TYPE_COND, ALT_VAL)					\
void __attribute__ ((noinline, noclone))				\
mask_slp_##TYPE_COND##_4_##ALT_VAL (int *restrict x, int *restrict y,	\
				    TYPE_COND *restrict z, int n)	\
{									\
  for (int i = 0; i < n; i += 4)					\
    {									\
      x[i] = y[i] ? z[i] : 1;						\
      x[i + 1] = y[i + 1] ? z[i + 1] : ALT_VAL;				\
      x[i + 2] = y[i + 2] ? z[i + 2] : 1;				\
      x[i + 3] = y[i + 3] ? z[i + 3] : ALT_VAL;				\
    }									\
}

#define MASK_SLP_8(TYPE_COND, ALT_VAL)					\
void __attribute__ ((noinline, noclone))				\
mask_slp_##TYPE_COND##_8_##ALT_VAL (int *restrict x, int *restrict y,	\
				    TYPE_COND *restrict z, int n)	\
{									\
  for (int i = 0; i < n; i += 8)					\
    {									\
      x[i] = y[i] ? z[i] : 1;						\
      x[i + 1] = y[i + 1] ? z[i + 1] : ALT_VAL;				\
      x[i + 2] = y[i + 2] ? z[i + 2] : 1;				\
      x[i + 3] = y[i + 3] ? z[i + 3] : ALT_VAL;				\
      x[i + 4] = y[i + 4] ? z[i + 4] : 1;				\
      x[i + 5] = y[i + 5] ? z[i + 5] : ALT_VAL;				\
      x[i + 6] = y[i + 6] ? z[i + 6] : 1;				\
      x[i + 7] = y[i + 7] ? z[i + 7] : ALT_VAL;				\
    }									\
}

#define MASK_SLP_FAIL(TYPE_COND)					\
void __attribute__ ((noinline, noclone))				\
mask_slp_##TYPE_COND##_FAIL (int *restrict x, int *restrict y,		\
			     TYPE_COND *restrict z, int n)		\
{									\
  for (int i = 0; i < n; i += 2)					\
    {									\
      x[i] = y[i] ? z[i] : 1;						\
      x[i + 1] = y[i + 1] ? z[i + 1] : x[z[i + 1]];			\
    }									\
}

MASK_SLP_2(int8_t, 1)
MASK_SLP_2(int8_t, 2)
MASK_SLP_2(int, 1)
MASK_SLP_2(int, 2)
MASK_SLP_2(int64_t, 1)
MASK_SLP_2(int64_t, 2)

MASK_SLP_4(int8_t, 1)
MASK_SLP_4(int8_t, 2)
MASK_SLP_4(int, 1)
MASK_SLP_4(int, 2)
MASK_SLP_4(int64_t, 1)
MASK_SLP_4(int64_t, 2)

MASK_SLP_8(int8_t, 1)
MASK_SLP_8(int8_t, 2)
MASK_SLP_8(int, 1)
MASK_SLP_8(int, 2)
MASK_SLP_8(int64_t, 1)
MASK_SLP_8(int64_t, 2)

MASK_SLP_FAIL(int8_t)
MASK_SLP_FAIL(int)
MASK_SLP_FAIL(int64_t)

/* { dg-final { scan-assembler-not {\tld2w\t} } } */
/* { dg-final { scan-assembler-not {\tst2w\t} } } */
/* { dg-final { scan-assembler-times {\tld1w\t} 48 } } */
/* { dg-final { scan-assembler-times {\tst1w\t} 40 } } */
