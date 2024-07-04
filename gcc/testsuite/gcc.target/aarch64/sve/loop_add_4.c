/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -msve-vector-bits=scalable" } */

#include <stdint.h>

#define LOOP(TYPE, NAME, STEP)					\
  __attribute__((noinline, noclone))				\
  void								\
  test_##TYPE##_##NAME (TYPE *dst, TYPE base, int count)	\
  {								\
    for (int i = 0; i < count; ++i, base += STEP)		\
      dst[i] += base;						\
  }

#define TEST_TYPE(T, TYPE) \
  T (TYPE, m17, -17) \
  T (TYPE, m16, -16) \
  T (TYPE, m15, -15) \
  T (TYPE, m1, -1) \
  T (TYPE, 1, 1) \
  T (TYPE, 15, 15) \
  T (TYPE, 16, 16) \
  T (TYPE, 17, 17)

#define TEST_ALL(T) \
  TEST_TYPE (T, int8_t) \
  TEST_TYPE (T, int16_t) \
  TEST_TYPE (T, int32_t) \
  TEST_TYPE (T, int64_t)

TEST_ALL (LOOP)

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.b, w[0-9]+, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1b\tz[0-9]+\.b, p[0-7]+/z, \[x[0-9]+, x[0-9]+\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b, p[0-7]+, \[x[0-9]+, x[0-9]+\]} 8 } } */
/* { dg-final { scan-assembler-times {\tincb\tx[0-9]+\n} 8 } } */

/* { dg-final { scan-assembler-not {\tdecb\tz[0-9]+\.b} } } */
/* We don't need to increment the vector IV for steps -16 and 16, since the
   increment is always a multiple of 256.  */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b, z[0-9]+\.b, z[0-9]+\.b\n} 14 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.h, w[0-9]+, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.h, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 1\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 1\]} 8 } } */
/* { dg-final { scan-assembler-times {\tincb\tx[0-9]+\n} 8 } } */

/* { dg-final { scan-assembler-times {\tdech\tz[0-9]+\.h, all, mul #16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdech\tz[0-9]+\.h, all, mul #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdech\tz[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tinch\tz[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tinch\tz[0-9]+\.h, all, mul #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tinch\tz[0-9]+\.h, all, mul #16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.h, z[0-9]+\.h, z[0-9]+\.h\n} 10 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.s, w[0-9]+, w[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.s, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 2\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 2\]} 8 } } */
/* { dg-final { scan-assembler-times {\tincw\tx[0-9]+\n} 8 } } */

/* { dg-final { scan-assembler-times {\tdecw\tz[0-9]+\.s, all, mul #16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdecw\tz[0-9]+\.s, all, mul #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdecw\tz[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tincw\tz[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tincw\tz[0-9]+\.s, all, mul #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tincw\tz[0-9]+\.s, all, mul #16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.s, z[0-9]+\.s, z[0-9]+\.s\n} 10 } } */

/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #-16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #-15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tindex\tz[0-9]+\.d, x[0-9]+, x[0-9]+\n} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz[0-9]+\.d, p[0-7]+/z, \[x[0-9]+, x[0-9]+, lsl 3\]} 8 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7]+, \[x[0-9]+, x[0-9]+, lsl 3\]} 8 } } */
/* { dg-final { scan-assembler-times {\tincd\tx[0-9]+\n} 8 } } */

/* { dg-final { scan-assembler-times {\tdecd\tz[0-9]+\.d, all, mul #16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdecd\tz[0-9]+\.d, all, mul #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tdecd\tz[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tincd\tz[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tincd\tz[0-9]+\.d, all, mul #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tincd\tz[0-9]+\.d, all, mul #16\n} 1 } } */
/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.d, z[0-9]+\.d, z[0-9]+\.d\n} 10 } } */
