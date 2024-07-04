/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>
#include "test-math.h"

/*
** test_double_int64_t___builtin_llceil:
**   frrm\s+[atx][0-9]+
**   ...
**   fsrmi\s+3
**   ...
**   vfcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+
**   ...
**   fsrm\s+[atx][0-9]+
**   ret
*/
TEST_UNARY_CALL_CVT (double, int64_t, __builtin_llceil)
