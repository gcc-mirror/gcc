/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 -fno-trapping-math" } */

#include <stdint.h>

#define COND_CVT(TYPE0, TYPE1, TYPE2, COUNT)		    \
  void                                                      \
  test_##TYPE0##_##TYPE1##_##TYPE2 (TYPE0 *__restrict out,  \
				    TYPE1 *__restrict a,    \
				    TYPE0 *__restrict b,    \
				    TYPE2 *__restrict p)    \
  {							    \
    for (unsigned int i = 0; i < COUNT; i++)                \
      out[i] = p[i] ?  (TYPE0)a[i] : b[i];		    \
  }

#define TEST_CVTF(PFX, T)		   \
  T (_Float16, PFX##int16_t, uint64_t, 32) \
  T (_Float16, PFX##int16_t, uint32_t, 64) \
  T (_Float16, PFX##int32_t, uint64_t, 32) \
  T (_Float16, PFX##int32_t, uint32_t, 64) \
  T (_Float16, PFX##int64_t, uint64_t, 32) \
  T (float, PFX##int32_t, uint64_t, 32)    \
  T (float, PFX##int64_t, uint64_t, 32)

#define TEST_ALL(T) \
  TEST_CVTF (, T)   \
  TEST_CVTF (u, T)

TEST_ALL (COND_CVT)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 8 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 6 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 8 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tscvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tucvtf\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.d\n} 1 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
