/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_U_ADD_IMM_TYPE_CHECK_FMT_2(uint16_t, 9)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" } } */
