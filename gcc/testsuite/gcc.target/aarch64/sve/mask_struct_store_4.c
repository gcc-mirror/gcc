/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */

#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)		\
  void __attribute__ ((noinline, noclone))			\
  NAME##_2 (OUTTYPE *__restrict dest, INTYPE *__restrict src,	\
	    MASKTYPE *__restrict cond, int n)			\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	if (cond[i] < 8)					\
	  dest[i * 2] = src[i];					\
	if (cond[i] > 2)					\
	  dest[i * 2 + 1] = src[i];				\
	}							\
  }

#define TEST2(NAME, OUTTYPE, INTYPE) \
  TEST_LOOP (NAME##_i8, OUTTYPE, INTYPE, signed char) \
  TEST_LOOP (NAME##_i16, OUTTYPE, INTYPE, unsigned short) \
  TEST_LOOP (NAME##_f32, OUTTYPE, INTYPE, float) \
  TEST_LOOP (NAME##_f64, OUTTYPE, INTYPE, double)

#define TEST1(NAME, OUTTYPE) \
  TEST2 (NAME##_i8, OUTTYPE, signed char) \
  TEST2 (NAME##_i16, OUTTYPE, unsigned short) \
  TEST2 (NAME##_i32, OUTTYPE, int) \
  TEST2 (NAME##_i64, OUTTYPE, unsigned long)

#define TEST(NAME) \
  TEST1 (NAME##_i8, signed char) \
  TEST1 (NAME##_i16, unsigned short) \
  TEST1 (NAME##_i32, int) \
  TEST1 (NAME##_i64, unsigned long) \
  TEST2 (NAME##_f16_f16, _Float16, _Float16) \
  TEST2 (NAME##_f32_f32, float, float) \
  TEST2 (NAME##_f64_f64, double, double)

TEST (test)

/* { dg-final { scan-assembler-not {\tst2b\t.z[0-9]} } } */
/* { dg-final { scan-assembler-not {\tst2h\t.z[0-9]} } } */
/* { dg-final { scan-assembler-not {\tst2w\t.z[0-9]} } } */
/* { dg-final { scan-assembler-not {\tst2d\t.z[0-9]} } } */
