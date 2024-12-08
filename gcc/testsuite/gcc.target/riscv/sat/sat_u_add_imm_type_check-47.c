/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_U_ADD_IMM_TYPE_CHECK_FMT_2(uint32_t, 4994967342ull)

/* { dg-final { scan-tree-dump-not ".SAT_ADD " "optimized" } } */
