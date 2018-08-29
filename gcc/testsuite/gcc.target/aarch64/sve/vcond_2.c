/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize" } */

#include <stdint.h>

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
  T (_Float16, int16_t, COND, SUFFIX##_float16)		\
  T (float, int32_t, COND, SUFFIX##_float)		\
  T (double, int64_t, COND, SUFFIX##_double)

#define TEST_COND_VAR_UNSIGNED_ALL(T, COND, SUFFIX)	\
  T (uint8_t, uint8_t, COND, SUFFIX)			\
  T (uint16_t, uint16_t, COND, SUFFIX)			\
  T (uint32_t, uint32_t, COND, SUFFIX)			\
  T (uint64_t, uint64_t, COND, SUFFIX)			\
  T (_Float16, uint16_t, COND, SUFFIX##_float16)	\
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
  T (_Float16, int16_t, COND, IMM, SUFFIX##_float16)	\
  T (float, int32_t, COND, IMM, SUFFIX##_float)		\
  T (double, int64_t, COND, IMM, SUFFIX##_double)

#define TEST_COND_IMM_UNSIGNED_ALL(T, COND, IMM, SUFFIX)	\
  T (uint8_t, uint8_t, COND, IMM, SUFFIX)			\
  T (uint16_t, uint16_t, COND, IMM, SUFFIX)			\
  T (uint32_t, uint32_t, COND, IMM, SUFFIX)			\
  T (uint64_t, uint64_t, COND, IMM, SUFFIX)			\
  T (_Float16, uint16_t, COND, IMM, SUFFIX##_float16)		\
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

/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.b, p[0-7], z[0-9]+\.b, z[0-9]+\.b\n} 66 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.h, p[0-7], z[0-9]+\.h, z[0-9]+\.h\n} 132 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.s, p[0-7], z[0-9]+\.s, z[0-9]+\.s\n} 132 } } */
/* { dg-final { scan-assembler-times {\tsel\tz[0-9]+\.d, p[0-7], z[0-9]+\.d, z[0-9]+\.d\n} 132 } } */

/* There are two signed ordered register comparisons for .b, one for a
   variable comparison and one for one of the two out-of-range constant
   comparisons.  The other out-of-ranger constant comparison can be
   adjusted to an in-range value by inverting the handling of equality.

   The same pattern appears twice for .h, .s and .d, once for integer data
   and once for floating-point data.  */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 4 } } */

/* Out-of-range >= is converted to in-range >.  */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmphs\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* Out-of-range < is converted to in-range <=.  */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmplo\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 2 } } */

/* 6 for .b: {signed, unsigned\n} x {variable, too high, too low}.  */
/* 12 for .h,.s and .d: the above 6 repeated for integer and floating-point
   data.  */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 6 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 12 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 12 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 12 } } */

/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, z[0-9]+\.b\n} 6 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, z[0-9]+\.h\n} 12 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, z[0-9]+\.s\n} 12 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, z[0-9]+\.d\n} 12 } } */

/* Also used for >= 16. */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #15\n} 4 } } */

/* gcc converts "a < 15" into "a <= 14".  */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #14\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #14\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #14\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #14\n} 2 } } */

/* gcc converts "a >= 15" into "a > 14".  */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #14\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #14\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #14\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpgt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #14\n} 2 } } */

/* Also used for < 16.  */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmple\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #15\n} 4 } } */

/* Appears once for each signedness.  */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #15\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #15\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #15\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} 4 } } */

/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} 4 } } */

/* gcc converts "a > -16" into "a >= -15".  */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #-15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #-15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #-15\n} 2 } } */

/* Also used for <= -17.  */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} 4 } } */

/* Also used for > -17.  */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #-16\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #-16\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpge\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #-16\n} 4 } } */

/* gcc converts "a <= -16" into "a < -15".  */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #-15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #-15\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmplt\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #-15\n} 2 } } */

/* gcc converts "a > 0" into "a != 0".  */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpne\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\n} 2 } } */

/* gcc converts "a <= 0" into "a == 0".  */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpeq\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #0\n} 2 } } */

/* Also used for >= 128.  */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #127\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #127\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #127\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #127\n} 4 } } */

/* gcc converts "a < 127" into "a <= 126".  */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #126\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #126\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #126\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #126\n} 2 } } */

/* gcc converts "a >= 127" into "a > 126".  */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #126\n} 1 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #126\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #126\n} 2 } } */
/* { dg-final { scan-assembler-times {\tcmphi\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #126\n} 2 } } */

/* Also used for < 128.  */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.b, p[0-7]/z, z[0-9]+\.b, #127\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.h, p[0-7]/z, z[0-9]+\.h, #127\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.s, p[0-7]/z, z[0-9]+\.s, #127\n} 4 } } */
/* { dg-final { scan-assembler-times {\tcmpls\tp[0-7]\.d, p[0-7]/z, z[0-9]+\.d, #127\n} 4 } } */
