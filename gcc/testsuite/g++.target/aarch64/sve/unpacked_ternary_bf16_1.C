/* { dg-do compile } */
/* { dg-options "-O2 -msve-vector-bits=2048" } */

#define BFMLA(TYPE)                               \
  TYPE test_bfmla_##TYPE (TYPE a, TYPE b, TYPE c) \
  { return a * b + c; }

#define BFMLS(TYPE)                               \
  TYPE test_bfmls_##TYPE (TYPE a, TYPE b, TYPE c) \
  { return a * -b + c; }

#define TEST_TYPE(TYPE, SIZE)                                 \
  typedef TYPE TYPE##SIZE __attribute__((vector_size(SIZE))); \
  BFMLA (TYPE##SIZE)                                          \
  BFMLS (TYPE##SIZE)

#pragma GCC target "arch=armv9-a+sve-b16b16"

TEST_TYPE (__bf16,  128)

TEST_TYPE (__bf16,  64)

/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.s} 2 } } */
/* { dg-final { scan-assembler-times {\tptrue\tp[0-7]\.d} 2 } } */

/* { dg-final { scan-assembler-times {\tbfmla\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbfmls\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
