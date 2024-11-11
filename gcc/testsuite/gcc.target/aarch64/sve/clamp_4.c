// { dg-options "-O" }

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

#define TEST(TYPE)							\
  TYPE									\
  untied_##TYPE(TYPE a, TYPE b, TYPE c, TYPE d)				\
  {									\
    return svminnm_x(svptrue_b8(), svmaxnm_x(svptrue_b8(), b, c), d);	\
  }

TEST(svfloat16_t)
TEST(svfloat32_t)
TEST(svfloat64_t)

/* { dg-final { scan-assembler-times {\tfclamp\tz0\.h, z2\.h, z3\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfclamp\tz0\.s, z2\.s, z3\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfclamp\tz0\.d, z2\.d, z3\.d\n} 1 } } */

/* { dg-final { scan-assembler-times {\tmovprfx\tz0, z1\n} 3 } } */
