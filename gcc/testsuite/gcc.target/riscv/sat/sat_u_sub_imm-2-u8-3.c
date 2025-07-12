/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

DEF_SAT_U_SUB_IMM_FMT_2(uint8_t, 1)

/* { dg-final { scan-tree-dump-not ".SAT_SUB" "optimized" } } */
