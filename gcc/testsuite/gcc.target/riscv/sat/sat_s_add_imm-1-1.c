/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_1(0, int8_t, uint8_t, -129, INT8_MIN, INT8_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(1, int8_t, uint8_t, 128, INT8_MIN, INT8_MAX)

/* { dg-final { scan-rtl-dump-not ".SAT_ADD " "expand" } } */
