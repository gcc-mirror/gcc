/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

/* Integer tests.  */

#define COMPARE_REG(NAME, OP, TYPE) \
  TYPE \
  cmp_##NAME##_##TYPE##_reg (TYPE a, TYPE b) \
  { \
    return a OP b; \
  }

#define COMPARE_REG_AND_ZERO(NAME, OP, TYPE) \
  COMPARE_REG (NAME, OP, TYPE) \
  \
  TYPE \
  cmp_##NAME##_##TYPE##_zero (TYPE a) \
  { \
    return a OP (TYPE) {}; \
  }

#define COMPARE_TYPE(TYPE, COMPARE_ORDERED) \
  COMPARE_REG_AND_ZERO (eq, ==, TYPE) \
  COMPARE_REG_AND_ZERO (ne, !=, TYPE) \
  COMPARE_ORDERED (lt, <, TYPE) \
  COMPARE_ORDERED (le, <=, TYPE) \
  COMPARE_ORDERED (gt, >, TYPE) \
  COMPARE_ORDERED (ge, >=, TYPE)

#define TEST_TYPE(NAME, ELEM, COMPARE_ORDERED, SIZE)  \
  typedef ELEM NAME##SIZE __attribute__((vector_size(SIZE))); \
  COMPARE_TYPE (NAME##SIZE, COMPARE_ORDERED)

/* 64-bits vectors, not vectorized.  */
TEST_TYPE (vs8, __INT8_TYPE__, COMPARE_REG_AND_ZERO, 8)
TEST_TYPE (vu8, __UINT8_TYPE__, COMPARE_REG, 8)
TEST_TYPE (vs16, __INT16_TYPE__, COMPARE_REG_AND_ZERO, 8)
TEST_TYPE (vu16, __UINT16_TYPE__, COMPARE_REG, 8)
TEST_TYPE (vs32, __INT32_TYPE__, COMPARE_REG_AND_ZERO, 8)
TEST_TYPE (vu32, __UINT32_TYPE__, COMPARE_REG, 8)

/* 128-bits vectors.  */
TEST_TYPE (vs8, __INT8_TYPE__, COMPARE_REG_AND_ZERO, 16)
TEST_TYPE (vu8, __UINT8_TYPE__, COMPARE_REG, 16)
TEST_TYPE (vs16, __INT16_TYPE__, COMPARE_REG_AND_ZERO, 16)
TEST_TYPE (vu16, __UINT16_TYPE__, COMPARE_REG, 16)
TEST_TYPE (vs32, __INT32_TYPE__, COMPARE_REG_AND_ZERO, 16)
TEST_TYPE (vu32, __UINT32_TYPE__, COMPARE_REG, 16)

/* { 8 bits } x { eq, ne, lt, le, gt, ge, hi, cs }.
/* { dg-final { scan-assembler-times {\tvcmp.i8\teq, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i8\tne, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tlt, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tle, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tgt, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tge, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u8\thi, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u8\tcs, q[0-9]+, q[0-9]+\n} 2 } } */

/* { 16 bits } x { eq, ne, lt, le, gt, ge, hi, cs }.
/* { dg-final { scan-assembler-times {\tvcmp.i16\teq, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i16\tne, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tlt, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tle, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tgt, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tge, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u16\thi, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u16\tcs, q[0-9]+, q[0-9]+\n} 2 } } */

/* { 32 bits } x { eq, ne, lt, le, gt, ge, hi, cs }.
/* { dg-final { scan-assembler-times {\tvcmp.i32\teq, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i32\tne, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tlt, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tle, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tgt, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tge, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u32\thi, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u32\tcs, q[0-9]+, q[0-9]+\n} 2 } } */
