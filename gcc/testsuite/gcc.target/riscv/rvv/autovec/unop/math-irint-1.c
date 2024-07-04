/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "test-math.h"

/*
** test_double_int___builtin_irint:
**   ...
**   vfncvt\.x\.f\.w\s+v[0-9]+,\s*v[0-9]+
**   ...
*/
TEST_UNARY_CALL_CVT (double, int, __builtin_irint)
