/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fopenmp-simd -msve-vector-bits=256 -fno-tree-loop-distribute-patterns" } */

#ifndef N
#define N 32
#endif

#include <stdint.h>

#define TEST_LOOP(TYPE, VALUE)			\
  void						\
  test_##TYPE (TYPE *data)			\
  {						\
    _Pragma ("omp simd")			\
    for (int i = 0; i < N / sizeof (TYPE); ++i)	\
      data[i] = VALUE;				\
  }

TEST_LOOP (uint8_t, 1)
TEST_LOOP (int8_t, 2)
TEST_LOOP (uint16_t, 3)
TEST_LOOP (int16_t, 4)
TEST_LOOP (uint32_t, 5)
TEST_LOOP (int32_t, 6)
TEST_LOOP (uint64_t, 7)
TEST_LOOP (int64_t, 8)
TEST_LOOP (_Float16, 1.0f)
TEST_LOOP (float, 2.0f)
TEST_LOOP (double, 3.0)

/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, #1\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.b, #2\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, #3\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.h, #4\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, #5\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.s, #6\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmov\tz[0-9]+\.d, #8\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.h, #1\.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.s, #2\.0e\+0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmov\tz[0-9]+\.d, #3\.0e\+0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.b, vl32\n} 11 } } */

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b,} 2 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s,} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d,} 3 } } */

/* { dg-final { scan-assembler-not {\twhile} } } */
/* { dg-final { scan-assembler-not {\tb} } } */
/* { dg-final { scan-assembler-not {\tcmp} } } */
/* { dg-final { scan-assembler-not {\tindex} } } */
