/* { dg-do compile } */
/* { dg-options "-O2 -fno-signed-zeros -ffinite-math-only -msve-vector-bits=2048" } */

#pragma GCC target "arch=armv9-a+sve-b16b16"

#define ADD(a, b) a + b
#define SUB(a, b) a - b
#define MUL(a, b) a * b
#define MAX(a, b) (a > b) ? a : b
#define MIN(a, b) (a > b) ? b : a

#define TEST_OP(TYPE, OP)                                        \
  TYPE test_##TYPE##_##OP (TYPE a, TYPE b) { return OP (a, b); } \

#define TEST_ALL(TYPE, SIZE)                                        \
  typedef TYPE TYPE##SIZE __attribute__((vector_size(SIZE)));       \
  TEST_OP (TYPE##SIZE, ADD)                                         \
  TEST_OP (TYPE##SIZE, SUB)                                         \
  TEST_OP (TYPE##SIZE, MUL)                                         \
  TEST_OP (TYPE##SIZE, MIN)                                         \
  TEST_OP (TYPE##SIZE, MAX)

TEST_ALL (__bf16, 64)

TEST_ALL (__bf16, 128)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s} 5 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.d} 5 } } */

/* { dg-final { scan-assembler-times {\tbfadd\tz[0-9]+\.h, p[0-7]/m. z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfsub\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */

/* { dg-final { scan-assembler-times {\tbfminnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfmaxnm\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
