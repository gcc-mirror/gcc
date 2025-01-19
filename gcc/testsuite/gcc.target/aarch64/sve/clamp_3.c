// { dg-options "-O" }

#include <arm_sve.h>

#pragma GCC target "+sve2p1"

#define TEST(TYPE)							\
  TYPE									\
  tied1_##TYPE(TYPE a, TYPE b, TYPE c)					\
  {									\
    return svminnm_x(svptrue_b8(), svmaxnm_x(svptrue_b8(), a, b), c);	\
  }									\
									\
  TYPE									\
  tied2_##TYPE(TYPE a, TYPE b, TYPE c)					\
  {									\
    return svminnm_x(svptrue_b8(), svmaxnm_x(svptrue_b8(), b, a), c);	\
  }

TEST(svfloat16_t)
TEST(svfloat32_t)
TEST(svfloat64_t)

/* { dg-final { scan-assembler-times {\tfclamp\tz0\.h, z1\.h, z2\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfclamp\tz0\.s, z1\.s, z2\.s\n} 2 } } */
/* { dg-final { scan-assembler-times {\tfclamp\tz0\.d, z1\.d, z2\.d\n} 2 } } */

/* { dg-final { scan-assembler-not {\tmovprfx\t} } } */
