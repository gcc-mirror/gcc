/* { dg-do assemble { target aarch64_asm_sve_ok } } */
/* { dg-options "-O -msve-vector-bits=2048 -save-temps" } */

#include <stdint.h>

#define TEST_OP_IMM(TYPE, OP, NAME, AMT) \
  TYPE test##_##TYPE##_##NAME (TYPE a) { return a < AMT ? a : AMT; }

#define TEST_OP(TYPE, MINV, MAXV) \
  TYPE test##_##TYPE##_reg (TYPE a, TYPE b) { return a < b ? a : b; } \
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
TEST_TYPE (int16_t, 64, -128, 127)
TEST_TYPE (uint16_t, 64, 4, 255)

TEST_TYPE (int8_t, 128, -120, 120)
TEST_TYPE (uint8_t, 128, 5, 251)
TEST_TYPE (int16_t, 128, -128, 127)
TEST_TYPE (uint16_t, 128, 6, 255)
TEST_TYPE (int32_t, 128, -128, 127)
TEST_TYPE (uint32_t, 128, 7, 255)

/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, p[0-7]/m, z[0-9]+\.b, z[0-9]+\.b\n} 3 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #-100\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #-110\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #-120\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.h, z[0-9]+\.h, #-128\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.s, z[0-9]+\.s, #-128\n} 1 } } */

/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #50\n} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.h, z[0-9]+\.h, #50\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.s, z[0-9]+\.s, #50\n} 1 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #100\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #110\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.b, z[0-9]+\.b, #120\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.h, z[0-9]+\.h, #127\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tsmin\tz[0-9]+\.s, z[0-9]+\.s, #127\n} 1 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #2\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #3\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #4\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #5\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #6\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #7\n} 1 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #50\n} 3 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #50\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #50\n} 1 { xfail *-*-* } } } */

/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #250\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #251\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.b, z[0-9]+\.b, #253\n} 1 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.h, z[0-9]+\.h, #255\n} 2 { xfail *-*-* } } } */
/* { dg-final { scan-assembler-times {\tumin\tz[0-9]+\.s, z[0-9]+\.s, #255\n} 1 { xfail *-*-* } } } */
