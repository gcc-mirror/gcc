/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 -fno-trapping-math" } */

#include <stdint.h>

#define FMLA(SUFF)  __builtin_fma##SUFF (a[i], b[i], c[i])
#define FMLS(SUFF)  __builtin_fma##SUFF (a[i], -b[i], c[i])
#define FNMLA(SUFF) -FMLA (SUFF)
#define FNMLS(SUFF) -FMLS (SUFF)

#define a_i a[i]
#define b_i b[i]
#define c_i c[i]

#define TEST_FN(FN, TYPE0, TYPE1, COUNT, MERGE)			\
  void								\
  f_##TYPE0##_##TYPE1##_##MERGE (TYPE0 *__restrict out,		\
				 TYPE0 *__restrict a,		\
				 TYPE0 *__restrict b,		\
				 TYPE0 *__restrict c,		\
				 TYPE1 *__restrict p)		\
  {								\
    for (unsigned int i = 0; i < COUNT; i++)			\
      out[i] = p[i] ? FN : MERGE;				\
  }

#define TEST_ALL(FN, TYPE0, TYPE1, COUNT) \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, a_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, c_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, 0)

TEST_ALL (FNMLS (f16), _Float16, uint64_t, 32)

TEST_ALL (FNMLS (f16), _Float16, uint32_t, 64)

TEST_ALL (FNMLS (f32), float, uint64_t, 32)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 12 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 12 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 12 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/z, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfnmsb\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfnmls\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/z, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfnmsb\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfnmls\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
