/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -save-temps" } */

#include <stdint.h>

#define TEST_OP(TYPE) \
  TYPE \
  test##_##TYPE##_##AMT (TYPE a, TYPE b, TYPE c) \
  { \
    return a + b * c; \
  }

#define TEST_TYPE(TYPE, SIZE) \
  typedef TYPE TYPE##SIZE __attribute__((vector_size(SIZE))); \
  TEST_OP (TYPE##SIZE)

TEST_TYPE (int8_t, 32)
TEST_TYPE (uint8_t, 32)

TEST_TYPE (int8_t, 64)
TEST_TYPE (uint8_t, 64)
TEST_TYPE (int16_t, 64)
TEST_TYPE (uint16_t, 64)

TEST_TYPE (int8_t, 128)
TEST_TYPE (uint8_t, 128)
TEST_TYPE (int16_t, 128)
TEST_TYPE (uint16_t, 128)
TEST_TYPE (int32_t, 128)
TEST_TYPE (uint32_t, 128)

/* { dg-final { scan-assembler-times {\t(?:mla|mad)\tz[0-9]+\.b,} 6 } } */
/* { dg-final { scan-assembler-times {\t(?:mla|mad)\tz[0-9]+\.h,} 4 } } */
/* { dg-final { scan-assembler-times {\t(?:mla|mad)\tz[0-9]+\.s,} 2 } } */
