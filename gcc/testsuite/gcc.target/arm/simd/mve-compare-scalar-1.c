/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O3" } */

#define COMPARE_REG(NAME, OP, TYPE, SCALAR)	  \
  TYPE						  \
  cmp_##NAME##_##TYPE##_scalar (TYPE a, SCALAR b) \
  {						  \
    return a OP b;				  \
  }

#define COMPARE_TYPE(SCALAR, TYPE)				\
  COMPARE_REG (eq, ==, TYPE, SCALAR)				\
  COMPARE_REG (ne, !=, TYPE, SCALAR)				\
  COMPARE_REG (lt, <, TYPE, SCALAR)				\
  COMPARE_REG (le, <=, TYPE, SCALAR)				\
  COMPARE_REG (gt, >, TYPE, SCALAR)				\
  COMPARE_REG (ge, >=, TYPE, SCALAR)

#define TEST_TYPE(NAME, ELEM, SIZE)			      \
  typedef ELEM NAME##SIZE __attribute__((vector_size(SIZE))); \
  COMPARE_TYPE (ELEM, NAME##SIZE)

/* 64-bits vectors, not vectorized.  */
TEST_TYPE (vs8, __INT8_TYPE__, 8)
TEST_TYPE (vu8, __UINT8_TYPE__, 8)
TEST_TYPE (vs16, __INT16_TYPE__, 8)
TEST_TYPE (vu16, __UINT16_TYPE__, 8)
TEST_TYPE (vs32, __INT32_TYPE__, 8)
TEST_TYPE (vu32, __UINT32_TYPE__, 8)

/* 128-bits vectors.  */
TEST_TYPE (vs8, __INT8_TYPE__, 16)
TEST_TYPE (vu8, __UINT8_TYPE__, 16)
TEST_TYPE (vs16, __INT16_TYPE__, 16)
TEST_TYPE (vu16, __UINT16_TYPE__, 16)
TEST_TYPE (vs32, __INT32_TYPE__, 16)
TEST_TYPE (vu32, __UINT32_TYPE__, 16)

/* { 8 bits } x { eq, ne, lt, le, gt, ge, hi, cs }.
/* { dg-final { scan-assembler-times {\tvcmp.i8\teq, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i8\tne, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tlt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tle, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tgt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s8\tge, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u8\thi, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u8\tcs, q[0-9]+, q[0-9]+\n} 2 } } */

/* { 16 bits } x { eq, ne, lt, le, gt, ge, hi, cs }.
/* { dg-final { scan-assembler-times {\tvcmp.i16\teq, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i16\tne, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tlt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tle, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tgt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s16\tge, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u16\thi, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u16\tcs, q[0-9]+, q[0-9]+\n} 2 } } */

/* { 32 bits } x { eq, ne, lt, le, gt, ge, hi, cs }.
/* { dg-final { scan-assembler-times {\tvcmp.i32\teq, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.i32\tne, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tlt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tle, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tgt, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.s32\tge, q[0-9]+, q[0-9]+\n} 1 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u32\thi, q[0-9]+, q[0-9]+\n} 2 } } */
/* { dg-final { scan-assembler-times {\tvcmp.u32\tcs, q[0-9]+, q[0-9]+\n} 2 } } */
