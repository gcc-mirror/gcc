/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fno-unroll-loops -fno-vect-cost-model -mlasx" } */

#include <stdint-gcc.h>

#define DEF_VCOND_VAR(DATA_TYPE, CMP_TYPE, COND, SUFFIX, IMM)	\
  void __attribute__ ((noinline, noclone))			\
  vcond_var_##CMP_TYPE##_##SUFFIX (DATA_TYPE *__restrict__ r,	\
				   DATA_TYPE *__restrict__ x,	\
				   DATA_TYPE *__restrict__ y,	\
				   CMP_TYPE *__restrict__ a,	\
				   int n)			\
  {								\
    for (int i = 0; i < n; i++)					\
      {								\
	DATA_TYPE xval = x[i], yval = y[i];			\
	CMP_TYPE aval = a[i], bval = IMM;			\
	r[i] = aval COND bval ? xval : yval;			\
      }								\
  }

#define TEST_COND_VAR_SIGNED_ALL(T, COND, SUFFIX)	\
  T (int8_t, int8_t, COND, SUFFIX, 0)			\
  T (int16_t, int16_t, COND, SUFFIX, 0)			\
  T (int32_t, int32_t, COND, SUFFIX, 0)			\
  T (int64_t, int64_t, COND, SUFFIX, 0)			\
  T (float, int32_t, COND, SUFFIX##_float, 0)		\
  T (double, int64_t, COND, SUFFIX##_double, 0)

#define TEST_COND_VAR_UNSIGNED_ALL(T, COND, SUFFIX)	\
  T (uint8_t, uint8_t, COND, SUFFIX, 2)			\
  T (uint16_t, uint16_t, COND, SUFFIX, 2)		\
  T (uint32_t, uint32_t, COND, SUFFIX, 2)		\
  T (uint64_t, uint64_t, COND, SUFFIX, 2)		\
  T (float, uint32_t, COND, SUFFIX##_float, 2)		\
  T (double, uint64_t, COND, SUFFIX##_double, 2)

#define TEST_COND_VAR_ALL(T, COND, SUFFIX)	 \
  TEST_COND_VAR_SIGNED_ALL (T, COND, SUFFIX)	 \
  TEST_COND_VAR_UNSIGNED_ALL (T, COND, SUFFIX)

#define TEST_VAR_ALL(T)				\
  TEST_COND_VAR_ALL (T, <, _lt)			\
  TEST_COND_VAR_ALL (T, <=, _le)		\
  TEST_COND_VAR_ALL (T, ==, _eq)		\
  TEST_COND_VAR_ALL (T, !=, _ne)

TEST_VAR_ALL (DEF_VCOND_VAR)

/* { dg-final { scan-assembler-times {\txvslti\.b\t} 1 } } */
/* { dg-final { scan-assembler-times {\txvslti\.h\t} 1 } } */
/* { dg-final { scan-assembler-times {\txvslti\.w\t} 2 } } */
/* { dg-final { scan-assembler-times {\txvslti\.d\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvslti\.b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvslti\.h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvslti\.w\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvslti\.d\t} 2 } } */
/* { dg-final { scan-assembler-times {\txvslei\.b\t} 1 } } */
/* { dg-final { scan-assembler-times {\txvslei\.h\t} 1 } } */
/* { dg-final { scan-assembler-times {\txvslei\.w\t} 2 } } */
/* { dg-final { scan-assembler-times {\txvslei\.d\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvslei\.b\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvslei\.h\t} 1 } } */
/* { dg-final { scan-assembler-times {\tvslei\.w\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvslei\.d\t} 2 } } */
/* { dg-final { scan-assembler-times {\txvslei\.bu\t} 2 } } */
/* { dg-final { scan-assembler-times {\txvslei\.hu\t} 2 } } */
/* { dg-final { scan-assembler-times {\txvslei\.wu\t} 4 } } */
/* { dg-final { scan-assembler-times {\txvslei\.du\t} 4 } } */
/* { dg-final { scan-assembler-times {\tvslei\.bu\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvslei\.hu\t} 2 } } */
/* { dg-final { scan-assembler-times {\tvslei\.wu\t} 4 } } */
/* { dg-final { scan-assembler-times {\tvslei\.du\t} 4 } } */
/* { dg-final { scan-assembler-times {\txvseqi\.b\t} 4 } } */
/* { dg-final { scan-assembler-times {\txvseqi\.h\t} 4 } } */
/* { dg-final { scan-assembler-times {\txvseqi\.w\t} 8 } } */
/* { dg-final { scan-assembler-times {\txvseqi\.d\t} 8 } } */
/* { dg-final { scan-assembler-times {\tvseqi\.b\t} 4 } } */
/* { dg-final { scan-assembler-times {\tvseqi\.h\t} 4 } } */
/* { dg-final { scan-assembler-times {\tvseqi\.w\t} 8 } } */
/* { dg-final { scan-assembler-times {\tvseqi\.d\t} 8 } } */
