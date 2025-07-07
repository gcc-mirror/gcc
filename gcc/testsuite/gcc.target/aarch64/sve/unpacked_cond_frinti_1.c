/* { dg-do compile } */
/* { dg-options "-O2 -moverride=sve_width=2048 -ftree-vectorize -fno-trapping-math" } */

#include <stdint.h>

#define a_i a[i]
#define b_i b[i]

#define TEST_FN(FN, TYPE0, TYPE1, COUNT, MERGE)			\
  void								\
  f_##FN##_##TYPE0##_##TYPE1##_##MERGE (TYPE1 *__restrict p,	\
    					TYPE0 *__restrict out,	\
					TYPE0 *__restrict a,	\
					TYPE0 *__restrict b)	\
  {								\
    for (unsigned int i = 0; i < COUNT; i++)			\
      out[i] = p[i] ? FN (a[i]) : MERGE;			\
  }

#define TEST_ALL(FN, TYPE0, TYPE1, COUNT) \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, a_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i)

TEST_ALL (__builtin_nearbyintf16, _Float16, uint64_t, 32)

TEST_ALL (__builtin_nearbyintf16, _Float16, uint32_t, 64)

TEST_ALL (__builtin_nearbyintf32, float, uint64_t, 32)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 3 } } */

/* { dg-final { scan-assembler-times {\tfrinti\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfrinti\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
