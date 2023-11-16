/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-unroll-loops -fno-vect-cost-model -mlasx" } */

#include <stdint-gcc.h>

#define DEF_VCOND_VAR(DATA_TYPE, CMP_TYPE, COND, SUFFIX)	\
  void __attribute__ ((noinline, noclone))			\
  vcond_var_##CMP_TYPE##_##SUFFIX (DATA_TYPE *__restrict__ r,	\
				   DATA_TYPE *__restrict__ x,	\
				   DATA_TYPE *__restrict__ y,	\
				   CMP_TYPE *__restrict__ a,	\
				   CMP_TYPE *__restrict__ b,	\
				   int n)			\
  {								\
    for (int i = 0; i < n; i++)					\
      {								\
	DATA_TYPE xval = x[i], yval = y[i];			\
	CMP_TYPE aval = a[i], bval = b[i];			\
	r[i] = aval COND bval ? xval : yval;			\
      }								\
  }

#define TEST_COND_VAR_SIGNED_ALL(T, COND, SUFFIX)	\
  T (int8_t, int8_t, COND, SUFFIX)			\
  T (int16_t, int16_t, COND, SUFFIX)			\
  T (int32_t, int32_t, COND, SUFFIX)			\
  T (int64_t, int64_t, COND, SUFFIX)			\
  T (float, int32_t, COND, SUFFIX##_float)		\
  T (double, int64_t, COND, SUFFIX##_double)

#define TEST_COND_VAR_UNSIGNED_ALL(T, COND, SUFFIX)	\
  T (uint8_t, uint8_t, COND, SUFFIX)			\
  T (uint16_t, uint16_t, COND, SUFFIX)			\
  T (uint32_t, uint32_t, COND, SUFFIX)			\
  T (uint64_t, uint64_t, COND, SUFFIX)			\
  T (float, uint32_t, COND, SUFFIX##_float)		\
  T (double, uint64_t, COND, SUFFIX##_double)

#define TEST_COND_VAR_ALL(T, COND, SUFFIX)	\
  TEST_COND_VAR_SIGNED_ALL (T, COND, SUFFIX)	\
  TEST_COND_VAR_UNSIGNED_ALL (T, COND, SUFFIX)

#define TEST_VAR_ALL(T)				\
  TEST_COND_VAR_ALL (T, >, _gt)			\
  TEST_COND_VAR_ALL (T, <, _lt)			\
  TEST_COND_VAR_ALL (T, >=, _ge)		\
  TEST_COND_VAR_ALL (T, <=, _le)		\
  TEST_COND_VAR_ALL (T, ==, _eq)		\
  TEST_COND_VAR_ALL (T, !=, _ne)

TEST_VAR_ALL (DEF_VCOND_VAR)

/* { dg-final { scan-assembler-times {\txvslt\.b} 4 } } */
/* { dg-final { scan-assembler-times {\txvslt\.h} 4 } } */
/* { dg-final { scan-assembler-times {\txvslt\.w} 8 } } */
/* { dg-final { scan-assembler-times {\txvslt\.d} 8 } } */
/* { dg-final { scan-assembler-times {\txvsle\.b} 4 } } */
/* { dg-final { scan-assembler-times {\txvsle\.h} 4 } } */
/* { dg-final { scan-assembler-times {\txvsle\.w} 8 } } */
/* { dg-final { scan-assembler-times {\txvsle\.d} 8 } } */
/* { dg-final { scan-assembler-times {\txvseq\.b} 4 } } */
/* { dg-final { scan-assembler-times {\txvseq\.h} 4 } } */
/* { dg-final { scan-assembler-times {\txvseq\.w} 8 } } */
/* { dg-final { scan-assembler-times {\txvseq\.d} 8 } } */
