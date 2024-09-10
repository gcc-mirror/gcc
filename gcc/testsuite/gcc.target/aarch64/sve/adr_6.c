/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -save-temps" } */

#include <stdint.h>

#define TEST_OP_IMM(TYPE, AMT) \
  TYPE test##_##TYPE##_##AMT (TYPE a, TYPE b) { return a + b * AMT; }

#define TEST_OP(TYPE) \
  TEST_OP_IMM (TYPE, 2) \
  TEST_OP_IMM (TYPE, 4) \
  TEST_OP_IMM (TYPE, 8)

#define TEST_TYPE(TYPE, SIZE) \
  typedef TYPE TYPE##SIZE __attribute__((vector_size(SIZE))); \
  TEST_OP (TYPE##SIZE)

TEST_TYPE (int8_t, 32)
TEST_TYPE (uint8_t, 32)

TEST_TYPE (int8_t, 64)
TEST_TYPE (uint8_t, 64)
TEST_TYPE (int16_t, 64)
TEST_TYPE (uint16_t, 64)

/* These two can't use ADR.  */
TEST_TYPE (int8_t, 128)
TEST_TYPE (uint8_t, 128)
TEST_TYPE (int16_t, 128)
TEST_TYPE (uint16_t, 128)
TEST_TYPE (int32_t, 128)
TEST_TYPE (uint32_t, 128)

/* { dg-final { scan-assembler-times {\tadd\tz[0-9]+\.b,} 8 } } */
/* { dg-final { scan-assembler-times {\tlsl\tz[0-9]+\.b,} 4 } } */

/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.s, \[z[0-9]+\.s, z[0-9]+\.s, lsl #?1\]\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.s, \[z[0-9]+\.s, z[0-9]+\.s, lsl #?2\]\n} 4 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.s, \[z[0-9]+\.s, z[0-9]+\.s, lsl #?3\]\n} 4 } } */

/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, lsl #?1\]\n} 6 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, lsl #?2\]\n} 6 } } */
/* { dg-final { scan-assembler-times {\tadr\tz[0-9]+\.d, \[z[0-9]+\.d, z[0-9]+\.d, lsl #?3\]\n} 6 } } */
