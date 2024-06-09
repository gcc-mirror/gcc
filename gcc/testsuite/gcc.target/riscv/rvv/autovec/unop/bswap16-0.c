/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize -fno-vect-cost-model -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint-gcc.h>
#include "test-math.h"

/*
** test_uint16_t___builtin_bswap16:
**   ...
**   vsrl\.vi\s+v[0-9]+,\s*v[0-9],\s*8+
**   vsll\.vi\s+v[0-9]+,\s*v[0-9],\s*8+
**   vor\.vv\s+v[0-9]+,\s*v[0-9],\s*v[0-9]+
**   ...
*/
TEST_UNARY_CALL (uint16_t, __builtin_bswap16)
