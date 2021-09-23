/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -save-temps" } */

#include <stdint.h>

#define TEST_OP_IMM(TYPE, OP, NAME, AMT) \
  TYPE test##_##TYPE##_##NAME (TYPE a) { return a * AMT; }

#define TEST_OP(TYPE, MINV, MAXV) \
  TYPE test##_##TYPE##_reg (TYPE a, TYPE b) { return a * b; } \
  TEST_OP_IMM (TYPE, OP, a, MINV) \
  TEST_OP_IMM (TYPE, OP, b, 50) \
  TEST_OP_IMM (TYPE, OP, c, MAXV)

#define TEST_TYPE(TYPE, SIZE, MINV, MAXV) \
  typedef TYPE TYPE##SIZE __attribute__((vector_size(SIZE))); \
  TEST_OP (TYPE##SIZE, MINV, MAXV)

TEST_TYPE (int8_t, 32, -100, 100)
TEST_TYPE (uint8_t, 32, 2, 250)

TEST_TYPE (int8_t, 64, -110, 110)
TEST_TYPE (uint8_t, 64, 3, 253)
TEST_TYPE (int16_t, 64, -123, 123)
TEST_TYPE (uint16_t, 64, 3, 255)

TEST_TYPE (int8_t, 128, -119, 120)
TEST_TYPE (uint8_t, 128, 4, 251)
TEST_TYPE (int16_t, 128, -123, 123)
TEST_TYPE (uint16_t, 128, 2, 255)
TEST_TYPE (int32_t, 128, -123, 123)
TEST_TYPE (uint32_t, 128, 4, 255)

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 6 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 4 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-100\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-110\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #-119\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #-123\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #-123\n} 1 } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #50\n} 6 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #50\n} 4 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #50\n} 2 } } */

/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #100\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #110\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.b, z[0-9]+\.b, #120\n} 1 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.h, z[0-9]+\.h, #123\n} 2 } } */
/* { dg-final { scan-assembler-times {\tmul\tz[0-9]+\.s, z[0-9]+\.s, #123\n} 1 } } */
