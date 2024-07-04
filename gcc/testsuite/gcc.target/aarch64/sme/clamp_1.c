// { dg-options "-O" }

#include <arm_sme.h>

#define TEST(TYPE)							\
  TYPE									\
  tied1_##TYPE(TYPE a, TYPE b, TYPE c) __arm_streaming			\
  {									\
    return svmin_x(svptrue_b8(), svmax_x(svptrue_b8(), a, b), c);	\
  }									\
									\
  TYPE									\
  tied2_##TYPE(TYPE a, TYPE b, TYPE c) __arm_streaming			\
  {									\
    return svmin_x(svptrue_b8(), svmax_x(svptrue_b8(), b, a), c);	\
  }

TEST(svint8_t)
TEST(svint16_t)
TEST(svint32_t)
TEST(svint64_t)

TEST(svuint8_t)
TEST(svuint16_t)
TEST(svuint32_t)
TEST(svuint64_t)

/* { dg-final { scan-assembler-times {\tsclamp\tz0\.b, z1\.b, z2\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsclamp\tz0\.h, z1\.h, z2\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsclamp\tz0\.s, z1\.s, z2\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tsclamp\tz0\.d, z1\.d, z2\.d\n} 2 } } */

/* { dg-final { scan-assembler-times {\tuclamp\tz0\.b, z1\.b, z2\.b\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuclamp\tz0\.h, z1\.h, z2\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuclamp\tz0\.s, z1\.s, z2\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuclamp\tz0\.d, z1\.d, z2\.d\n} 2 } } */

/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
