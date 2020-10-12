/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1" }  */
/* { dg-add-options arm_neon } */

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

#define TEST_TYPE(NAME, ELEM, COMPARE_ORDERED) \
  typedef ELEM NAME __attribute__((vector_size(16))); \
  COMPARE_TYPE (NAME, COMPARE_ORDERED)

TEST_TYPE (vs8, __INT8_TYPE__, COMPARE_REG_AND_ZERO)
TEST_TYPE (vu8, __UINT8_TYPE__, COMPARE_REG)
TEST_TYPE (vs16, __INT16_TYPE__, COMPARE_REG_AND_ZERO)
TEST_TYPE (vu16, __UINT16_TYPE__, COMPARE_REG)
TEST_TYPE (vs32, __INT32_TYPE__, COMPARE_REG_AND_ZERO)
TEST_TYPE (vu32, __UINT32_TYPE__, COMPARE_REG)

/* { s8, u8 } x { eq, ne }.
/* { dg-final { scan-assembler-times {\tvceq.i8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i8\tq[0-9]+, q[0-9]+, #0\n} 4 } } */

/* { dg-final { scan-assembler-times {\tvcgt.s8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvclt.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcge.s8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s8\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcgt.u8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u8\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */

/* { s16, u16 } x { eq, ne }.
/* { dg-final { scan-assembler-times {\tvceq.i16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i16\tq[0-9]+, q[0-9]+, #0\n} 4 } } */

/* { dg-final { scan-assembler-times {\tvcgt.s16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvclt.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcge.s16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s16\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcgt.u16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u16\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */

/* { s32, u32 } x { eq, ne }.
/* { dg-final { scan-assembler-times {\tvceq.i32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 4 } } */
/* { dg-final { scan-assembler-times {\tvceq.i32\tq[0-9]+, q[0-9]+, #0\n} 4 } } */

/* { dg-final { scan-assembler-times {\tvcgt.s32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcgt.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvclt.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcge.s32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcle.s32\tq[0-9]+, q[0-9]+, #0\n} 1 } } */

/* { dg-final { scan-assembler-times {\tvcgt.u32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcge.u32\tq[0-9]+, q[0-9]+, q[0-9]+\n} 2 } } */
