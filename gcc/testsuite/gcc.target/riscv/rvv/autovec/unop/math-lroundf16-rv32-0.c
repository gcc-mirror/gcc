/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvfh_zfh -mabi=ilp32d -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "test-math.h"

/*
** test__Float16_long___builtin_lroundf16:
**   frrm\s+[atx][0-9]+
**   ...
**   fsrmi\s+4
**   ...
**   vfwcvt\.x\.f\.v\s+v[0-9]+,\s*v[0-9]+
**   ...
**   fsrm\s+[atx][0-9]+
**   ret
*/
TEST_UNARY_CALL_CVT (_Float16, long, __builtin_lroundf16)
