/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math --param aarch64-sve-compare-costs=0" } */

#include <stdint.h>

#define TEST_LOOP(NAME, OUTTYPE, INTYPE, MASKTYPE)		\
  void __attribute__ ((noinline, noclone))			\
  NAME##_2 (OUTTYPE *__restrict dest, INTYPE *__restrict src,	\
	    MASKTYPE *__restrict cond, intptr_t n)		\
  {								\
    for (intptr_t i = 0; i < n; ++i)				\
      if (cond[i])						\
	dest[i] = src[i * 2] + src[i * 2 + 1];			\
  }

#define TEST2(NAME, OUTTYPE, INTYPE) \
  TEST_LOOP (NAME##_i8, OUTTYPE, INTYPE, int8_t) \
  TEST_LOOP (NAME##_i16, OUTTYPE, INTYPE, uint16_t) \
  TEST_LOOP (NAME##_f32, OUTTYPE, INTYPE, float) \
  TEST_LOOP (NAME##_f64, OUTTYPE, INTYPE, double)

#define TEST1(NAME, OUTTYPE) \
  TEST2 (NAME##_i8, OUTTYPE, int8_t) \
  TEST2 (NAME##_i16, OUTTYPE, uint16_t) \
  TEST2 (NAME##_i32, OUTTYPE, int32_t) \
  TEST2 (NAME##_i64, OUTTYPE, uint64_t)

#define TEST(NAME) \
  TEST1 (NAME##_i8, int8_t) \
  TEST1 (NAME##_i16, uint16_t) \
  TEST1 (NAME##_i32, int32_t) \
  TEST1 (NAME##_i64, uint64_t) \
  TEST2 (NAME##_f16_f16, _Float16, _Float16) \
  TEST2 (NAME##_f32_f32, float, float) \
  TEST2 (NAME##_f64_f64, double, double)

TEST (test)

/*    Mask |  8 16 32 64
    -------+------------
    Out  8 |  1  1  1  1
        16 |  1  1  1  1
        32 |  1  1  1  1
        64 |  1  1  1  1.  */
/* { dg-final { scan-assembler-times {\tld2b\t.z[0-9]} 16 } } */

/*    Mask |  8 16 32 64
    -------+------------
    Out  8 |  2  2  2  2
        16 |  2  1  1  1 x2 (for half float)
        32 |  2  1  1  1
        64 |  2  1  1  1.  */
/* { dg-final { scan-assembler-times {\tld2h\t.z[0-9]} 28 } } */

/*    Mask |  8 16 32 64
    -------+------------
    Out  8 |  4  4  4  4
        16 |  4  2  2  2
        32 |  4  2  1  1 x2 (for float)
        64 |  4  2  1  1.  */
/* { dg-final { scan-assembler-times {\tld2w\t.z[0-9]} 50 } } */

/*    Mask |  8 16 32 64
    -------+------------
    Out  8 |  8  8  8  8
        16 |  8  4  4  4
        32 |  8  4  2  2
        64 |  8  4  2  1 x2 (for double).  */
/* { dg-final { scan-assembler-times {\tld2d\t.z[0-9]} 98 } } */
