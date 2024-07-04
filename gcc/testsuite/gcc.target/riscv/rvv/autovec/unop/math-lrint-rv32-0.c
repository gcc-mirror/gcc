/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "test-math.h"

/*
** test_double_long___builtin_lrint:
**   ...
**   vfncvt\.x\.f\.w\s+v[0-9]+,\s*v[0-9]+
**   ...
*/
TEST_UNARY_CALL_CVT (double, long, __builtin_lrint)
