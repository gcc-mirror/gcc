// { dg-options "-O" }

#include <arm_sme.h>

#pragma GCC target "+nosme2"

#define TEST(TYPE)							\
  TYPE									\
  tied1_##TYPE(TYPE a, TYPE b, TYPE c) __arm_streaming			\
  {									\
    return svminnm_x(svptrue_b8(), svmaxnm_x(svptrue_b8(), a, b), c);	\
  }									\
									\
  TYPE									\
  tied2_##TYPE(TYPE a, TYPE b, TYPE c) __arm_streaming			\
  {									\
    return svminnm_x(svptrue_b8(), svmaxnm_x(svptrue_b8(), b, a), c);	\
  }

TEST(svfloat16_t)
TEST(svfloat32_t)
TEST(svfloat64_t)

/* { dg-final { scan-assembler-not {\tfclamp\t} } } */
