/* { dg-do compile } */
/* { dg-additional-options "-march=rv32gcv -mabi=ilp32d -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

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

#define DEF_VCOND_IMM(DATA_TYPE, CMP_TYPE, COND, IMM, SUFFIX)	\
  void __attribute__ ((noinline, noclone))			\
  vcond_imm_##CMP_TYPE##_##SUFFIX (DATA_TYPE *__restrict__ r,	\
				   DATA_TYPE *__restrict__ x,	\
				   DATA_TYPE *__restrict__ y,	\
				   CMP_TYPE *__restrict__ a,	\
				   int n)			\
  {								\
    for (int i = 0; i < n; i++)					\
      {								\
	DATA_TYPE xval = x[i], yval = y[i];			\
	CMP_TYPE aval = a[i];					\
	r[i] = aval COND (CMP_TYPE) IMM ? xval : yval;		\
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

#define TEST_COND_IMM_SIGNED_ALL(T, COND, IMM, SUFFIX)	\
  T (int8_t, int8_t, COND, IMM, SUFFIX)			\
  T (int16_t, int16_t, COND, IMM, SUFFIX)		\
  T (int32_t, int32_t, COND, IMM, SUFFIX)		\
  T (int64_t, int64_t, COND, IMM, SUFFIX)		\
  T (float, int32_t, COND, IMM, SUFFIX##_float)		\
  T (double, int64_t, COND, IMM, SUFFIX##_double)

#define TEST_COND_IMM_UNSIGNED_ALL(T, COND, IMM, SUFFIX)	\
  T (uint8_t, uint8_t, COND, IMM, SUFFIX)			\
  T (uint16_t, uint16_t, COND, IMM, SUFFIX)			\
  T (uint32_t, uint32_t, COND, IMM, SUFFIX)			\
  T (uint64_t, uint64_t, COND, IMM, SUFFIX)			\
  T (float, uint32_t, COND, IMM, SUFFIX##_float)		\
  T (double, uint64_t, COND, IMM, SUFFIX##_double)

#define TEST_COND_IMM_ALL(T, COND, IMM, SUFFIX)		\
  TEST_COND_IMM_SIGNED_ALL (T, COND, IMM, SUFFIX)	\
  TEST_COND_IMM_UNSIGNED_ALL (T, COND, IMM, SUFFIX)

#define TEST_IMM_ALL(T)							\
  /* Expect immediates to make it into the encoding.  */		\
  TEST_COND_IMM_ALL (T, >, 5, _gt)					\
  TEST_COND_IMM_ALL (T, <, 5, _lt)					\
  TEST_COND_IMM_ALL (T, >=, 5, _ge)					\
  TEST_COND_IMM_ALL (T, <=, 5, _le)					\
  TEST_COND_IMM_ALL (T, ==, 5, _eq)					\
  TEST_COND_IMM_ALL (T, !=, 5, _ne)					\
									\
  TEST_COND_IMM_SIGNED_ALL (T, >, 15, _gt2)				\
  TEST_COND_IMM_SIGNED_ALL (T, <, 15, _lt2)				\
  TEST_COND_IMM_SIGNED_ALL (T, >=, 15, _ge2)				\
  TEST_COND_IMM_SIGNED_ALL (T, <=, 15, _le2)				\
  TEST_COND_IMM_ALL (T, ==, 15, _eq2)					\
  TEST_COND_IMM_ALL (T, !=, 15, _ne2)					\
									\
  TEST_COND_IMM_SIGNED_ALL (T, >, 16, _gt3)				\
  TEST_COND_IMM_SIGNED_ALL (T, <, 16, _lt3)				\
  TEST_COND_IMM_SIGNED_ALL (T, >=, 16, _ge3)				\
  TEST_COND_IMM_SIGNED_ALL (T, <=, 16, _le3)				\
  TEST_COND_IMM_ALL (T, ==, 16, _eq3)					\
  TEST_COND_IMM_ALL (T, !=, 16, _ne3)					\
									\
  TEST_COND_IMM_SIGNED_ALL (T, >, -16, _gt4)				\
  TEST_COND_IMM_SIGNED_ALL (T, <, -16, _lt4)				\
  TEST_COND_IMM_SIGNED_ALL (T, >=, -16, _ge4)				\
  TEST_COND_IMM_SIGNED_ALL (T, <=, -16, _le4)				\
  TEST_COND_IMM_ALL (T, ==, -16, _eq4)					\
  TEST_COND_IMM_ALL (T, !=, -16, _ne4)					\
									\
  TEST_COND_IMM_SIGNED_ALL (T, >, -17, _gt5)				\
  TEST_COND_IMM_SIGNED_ALL (T, <, -17, _lt5)				\
  TEST_COND_IMM_SIGNED_ALL (T, >=, -17, _ge5)				\
  TEST_COND_IMM_SIGNED_ALL (T, <=, -17, _le5)				\
  TEST_COND_IMM_ALL (T, ==, -17, _eq5)					\
  TEST_COND_IMM_ALL (T, !=, -17, _ne5)					\
									\
  TEST_COND_IMM_UNSIGNED_ALL (T, >, 0, _gt6)				\
  /* Testing if an unsigned value >= 0 or < 0 is pointless as it will	\
     get folded away by the compiler.  */				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <=, 0, _le6)				\
									\
  TEST_COND_IMM_UNSIGNED_ALL (T, >, 127, _gt7)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <, 127, _lt7)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, >=, 127, _ge7)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <=, 127, _le7)				\
									\
  /* Expect immediates to NOT make it into the encoding, and instead be \
     forced into a register.  */					\
  TEST_COND_IMM_UNSIGNED_ALL (T, >, 128, _gt8)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <, 128, _lt8)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, >=, 128, _ge8)				\
  TEST_COND_IMM_UNSIGNED_ALL (T, <=, 128, _le8)

TEST_VAR_ALL (DEF_VCOND_VAR)
TEST_IMM_ALL (DEF_VCOND_IMM)

/* { dg-final { scan-assembler-times {\tvmseq\.vi} 42 } } */
/* { dg-final { scan-assembler-times {\tvmsne\.vi} 42 } } */
/* { dg-final { scan-assembler-times {\tvmsgt\.vi} 30 } } */
/* { dg-final { scan-assembler-times {\tvmsgtu\.vi} 12 } } */
/* { dg-final { scan-assembler-times {\tvmslt\.vi} 8 } } */
/* { dg-final { scan-assembler-times {\tvmsge\.vi} 8 } } */
/* { dg-final { scan-assembler-times {\tvmsle\.vi} 30 } } */
/* { dg-final { scan-assembler-times {\tvmsleu\.vi} 12 } } */
/* { dg-final { scan-assembler-times {\tvmseq} 78 } } */
/* { dg-final { scan-assembler-times {\tvmsne} 78 } } */
/* { dg-final { scan-assembler-times {\tvmsgt} 82 } } */
/* { dg-final { scan-assembler-times {\tvmslt} 38 } } */
/* { dg-final { scan-assembler-times {\tvmsge} 38 } } */
/* { dg-final { scan-assembler-times {\tvmsle} 82 } } */
