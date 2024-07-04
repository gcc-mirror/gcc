/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "test-math.h"

/*
** test_double_long___builtin_lround:
**   frrm\s+[atx][0-9]+
**   ...
**   fsrmi\s+4
**   ...
**   vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+
**   ...
**   fsrm\s+[atx][0-9]+
**   ret
*/
TEST_UNARY_CALL_CVT (double, long, __builtin_lround)
