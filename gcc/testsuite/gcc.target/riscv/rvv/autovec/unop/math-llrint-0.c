/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "test-math.h"

/*
** test_double_int64_t___builtin_llrint:
**   ...
**   vsetvli\s+[atx][0-9]+,\s*zero,\s*e64,\s*m1,\s*ta,\s*ma
**   vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+
**   ...
*/
TEST_UNARY_CALL_CVT (double, int64_t, __builtin_llrint)
