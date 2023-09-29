/* { dg-options "-O2" } */

#include <arm_sve.h>

#define TEST_OFFSET(TYPE, SUFFIX, OFFSET) \
  sv##TYPE##_t \
  test_##TYPE##_##SUFFIX (TYPE##_t *ptr) \
  { \
    return svld1rq(svptrue_b8(), ptr + OFFSET); \
  }

#define TEST(TYPE) \
  TEST_OFFSET (TYPE, 0, 0) \
  TEST_OFFSET (TYPE, 1, 1) \
  TEST_OFFSET (TYPE, 2, 2) \
  TEST_OFFSET (TYPE, 16, 16) \
  TEST_OFFSET (TYPE, 0x10000, 0x10000) \
  TEST_OFFSET (TYPE, 0x10001, 0x10001) \
  TEST_OFFSET (TYPE, m1, -1) \
  TEST_OFFSET (TYPE, m2, -2) \
  TEST_OFFSET (TYPE, m16, -16) \
  TEST_OFFSET (TYPE, m0x10000, -0x10000) \
  TEST_OFFSET (TYPE, m0x10001, -0x10001)

TEST (int8)
TEST (int16)
TEST (uint32)
TEST (uint64)

/* { dg-final { scan-assembler-times {\tld1rqb\t} 11 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tld1rqh\t} 11 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tld1rqw\t} 11 { target aarch64_little_endian } } } */
/* { dg-final { scan-assembler-times {\tld1rqd\t} 11 { target aarch64_little_endian } } } */
