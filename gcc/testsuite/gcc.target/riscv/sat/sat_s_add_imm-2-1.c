/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-rtl-expand-details" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_1(0, int16_t, uint16_t, -32769, INT16_MIN, INT16_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(1, int16_t, uint16_t, 32768, INT16_MIN, INT16_MAX)

/* { dg-final { scan-rtl-dump-not ".SAT_ADD " "expand" } } */
