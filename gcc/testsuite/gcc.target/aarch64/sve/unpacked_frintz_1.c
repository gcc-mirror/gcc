/* { dg-do compile } */
/* { dg-options "-O2 -moverride=sve_width=2048 -ftree-vectorize -fno-trapping-math" } */

#include <stdint.h>

#define TEST_FN(FN, TYPE0, TYPE1, COUNT)		\
  void							\
  f_##FN##_##TYPE0##_##TYPE1 (TYPE1 *__restrict out,	\
			      TYPE0 *__restrict a,      \
			      TYPE0 *__restrict b)      \
  {							\
    for (unsigned int i = 0; i < COUNT; i++)		\
      if (FN (a[i]) > b[i])				\
	out[i] = 3;					\
  }

TEST_FN (__builtin_truncf16, _Float16, uint64_t, 32)

TEST_FN (__builtin_truncf16, _Float16, uint32_t, 64)

TEST_FN (__builtin_truncf32, float, uint64_t, 32)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s} 0 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.d} 0 } } */

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 2 } } */

/* { dg-final { scan-assembler-times {\tfrintz\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfrintz\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
