/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 -fno-trapping-math" } */

#include <stdint.h>

#define a_i a[i]
#define b_i b[i]
#define c_i c[i]
#define imm_p5 0.5

#define MUL(A, B) A * B

#define TEST_FN(FN, TYPE0, TYPE1, COUNT, RHS, MERGE)		\
  void								\
  f_##TYPE0##_##TYPE1##_##RHS##_##MERGE (TYPE0 *__restrict out,	\
					 TYPE0 *__restrict a,	\
					 TYPE0 *__restrict b,	\
					 TYPE0 *__restrict c,	\
					 TYPE1 *__restrict p)	\
  {								\
    for (unsigned int i = 0; i < COUNT; i++)			\
      out[i] = p[i] ? FN (a[i], (TYPE0)RHS) : MERGE;		\
  }

#define TEST_ALL(FN, TYPE0, TYPE1, COUNT)	 \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i, a_i)    \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i, b_i)    \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i, c_i)    \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, imm_p5, a_i) \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, imm_p5, b_i)

TEST_ALL (MUL, _Float16, uint64_t, 32)

TEST_ALL (MUL, _Float16, uint32_t, 64)

TEST_ALL (MUL, float, uint64_t, 32)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 10 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 10 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 10 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
