// { dg-options "-O" }

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

#define TEST(TYPE)							\
  TYPE									\
  untied_##TYPE(TYPE a, TYPE b, TYPE c, TYPE d)				\
  {									\
    return svmin_x(svptrue_b8(), svmax_x(svptrue_b8(), b, c), d);	\
  }

TEST(svint8_t)
TEST(svint16_t)
TEST(svint32_t)
TEST(svint64_t)

TEST(svuint8_t)
TEST(svuint16_t)
TEST(svuint32_t)
TEST(svuint64_t)

/* { dg-final { scan-assembler-times {\tsclamp\tz0\.b, z2\.b, z3\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsclamp\tz0\.h, z2\.h, z3\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsclamp\tz0\.s, z2\.s, z3\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsclamp\tz0\.d, z2\.d, z3\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tuclamp\tz0\.b, z2\.b, z3\.b\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuclamp\tz0\.h, z2\.h, z3\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuclamp\tz0\.s, z2\.s, z3\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuclamp\tz0\.d, z2\.d, z3\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz0, z1\n} 8 } } */
