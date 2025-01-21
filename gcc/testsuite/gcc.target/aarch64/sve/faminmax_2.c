/* { dg-do compile } */
/* { dg-additional-options "-O3 -ffast-math" } */

#include "arm_sve.h"

#pragma GCC target "+sve2+faminmax"

#define TEST_WITH_SVMAX(TYPE)						\
  TYPE fn_fmax_##TYPE (TYPE x, TYPE y) {				\
    svbool_t pg = svptrue_b8();						\
    return svmax_x(pg, svabs_x(pg, x), svabs_x(pg, y));			\
  }									\

#define TEST_WITH_SVMAXNM(TYPE)						\
  TYPE fn_fmaxnm_##TYPE (TYPE x, TYPE y) {				\
    svbool_t pg = svptrue_b8();						\
    return svmaxnm_x(pg, svabs_x(pg, x), svabs_x(pg, y));		\
  }									\

#define TEST_WITH_SVMIN(TYPE)						\
  TYPE fn_fmin_##TYPE (TYPE x, TYPE y) {				\
    svbool_t pg = svptrue_b8();						\
    return svmin_x(pg, svabs_x(pg, x), svabs_x(pg, y));			\
  }									\

#define TEST_WITH_SVMINNM(TYPE)						\
  TYPE fn_fminnm_##TYPE (TYPE x, TYPE y) {				\
    svbool_t pg = svptrue_b8();						\
    return svminnm_x(pg, svabs_x(pg, x), svabs_x(pg, y));		\
  }									\

TEST_WITH_SVMAX (svfloat16_t)
TEST_WITH_SVMAX (svfloat32_t)
TEST_WITH_SVMAX (svfloat64_t)

TEST_WITH_SVMAXNM (svfloat16_t)
TEST_WITH_SVMAXNM (svfloat32_t)
TEST_WITH_SVMAXNM (svfloat64_t)

TEST_WITH_SVMIN (svfloat16_t)
TEST_WITH_SVMIN (svfloat32_t)
TEST_WITH_SVMIN (svfloat64_t)

TEST_WITH_SVMINNM (svfloat16_t)
TEST_WITH_SVMINNM (svfloat32_t)
TEST_WITH_SVMINNM (svfloat64_t)

/* { dg-final { scan-assembler-not {\tfamax\t} } }  */
/* { dg-final { scan-assembler-not {\tfamin\t} } }  */

/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h\n} 8 } } */
/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s\n} 8 } } */
/* { dg-final { scan-assembler-times {\tfabs\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d\n} 8 } } */

/* { dg-final { scan-assembler-times {\tfmax\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmax\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmax\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmin\tz[0-9]+\.h, p[0-7]/m, z[0-9]+\.h, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmin\tz[0-9]+\.s, p[0-7]/m, z[0-9]+\.s, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tfmin\tz[0-9]+\.d, p[0-7]/m, z[0-9]+\.d, z[0-9]+\.d\n} 1 } } */
