/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_U_ADD_IMM_TYPE_CHECK_FMT_1(uint64_t, 576460752303483482)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" } } */
