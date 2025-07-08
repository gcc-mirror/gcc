/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -moverride=sve_width=2048 -fno-trapping-math" } */

#include <stdint.h>

#define a_i a[i]
#define b_i b[i]
#define c_i c[i]
#define imm_p5 0.5

#define ADD(A, B) A + B

#define TEST_FN(FN, TYPE0, TYPE1, COUNT, NAME, RHS, MERGE)	 \
  void								 \
  f_##TYPE0##_##TYPE1##_##NAME##_##MERGE (TYPE0 *__restrict out, \
					 TYPE0 *__restrict a,	 \
					 TYPE0 *__restrict b,	 \
					 TYPE0 *__restrict c,	 \
					 TYPE1 *__restrict p)	 \
  {								 \
    for (unsigned int i = 0; i < COUNT; i++)			 \
      out[i] = p[i] ? FN (a[i], (TYPE0)RHS) : MERGE;		 \
  }

#define TEST_ALL(FN, TYPE0, TYPE1, COUNT)	     \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i, b[i], a_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i, b[i], b_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, b_i, b[i], c_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, one, 1, a_i)     \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, one, 1, b_i)     \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, none, -1, a_i)   \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, none, -1, b_i)   \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, p5, 0.5, a_i)    \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, p5, 0.5, b_i)    \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, np5, -0.5, a_i)  \
  TEST_FN (FN, TYPE0, TYPE1, COUNT, np5, -0.5, b_i)

TEST_ALL (ADD, _Float16, uint64_t, 32)

TEST_ALL (ADD, _Float16, uint32_t, 64)

TEST_ALL (ADD, float, uint64_t, 32)

/* { dg-final { scan-assembler-times {\tld1w\tz[0-9]+\.d} 19 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.s} 19 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz[0-9]+\.d} 19 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 5 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 3 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1.0\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #0.5\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, #1.0\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 10 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 6 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfadd\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1.0\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #0.5\n} 4 } } */
/* { dg-final { scan-assembler-times {\tfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, #1.0\n} 4 } } */

/* { dg-final { scan-assembler-not {\tsel\t} } } */
