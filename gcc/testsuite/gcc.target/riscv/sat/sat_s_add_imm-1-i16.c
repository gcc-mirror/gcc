/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_S_ADD_IMM_FMT_1(0, int16_t, uint16_t, -7, INT16_MIN, INT16_MAX)
DEF_SAT_S_ADD_IMM_FMT_1(1, int16_t, uint16_t, -1, INT16_MIN, INT16_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" } } */
/* { dg-final { scan-assembler-not "\.L\[0-9\]+" } } */
