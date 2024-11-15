/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_1(0, int32_t, uint32_t, -2147483649, INT32_MIN, INT32_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(1, int32_t, uint32_t, 2147483648, INT32_MIN, INT32_MAX)

/* { dg-final { scan-rtl-dump-not ".SAT_ADD " "expand" } } */
