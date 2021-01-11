/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -save-temps" } */

#include <stdint.h>

#define TEST_OP(TYPE) \
  TYPE \
  test##_##TYPE##_reg (TYPE a, TYPE b, TYPE c) \
  { \
    return c == 0 ? a & ~b : b; \
  }

#define TEST_TYPE(TYPE, SIZE) \
  typedef TYPE TYPE##SIZE __attribute__((vector_size(SIZE))); \
  TEST_OP (TYPE##SIZE)

TEST_TYPE (uint8_t, 32)

TEST_TYPE (uint8_t, 64)
TEST_TYPE (uint16_t, 64)

TEST_TYPE (uint8_t, 128)
TEST_TYPE (uint16_t, 128)
TEST_TYPE (uint32_t, 128)

/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tbic\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
/* { dg-final { scan-assembler-times {\tsel\t} 6 } } */
