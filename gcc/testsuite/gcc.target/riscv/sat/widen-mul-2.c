/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "widen-mul.h"

SAT_U_MUL_FMT_5(uint32_t, uint128_t)

/* { dg-final { scan-tree-dump-not " = (__int128 unsigned) " "optimized" } } */
