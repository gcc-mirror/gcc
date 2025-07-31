/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -fdump-tree-optimized" } */

#include "sat_arith.h"

#define NT uint64_t
#define WT uint128_t

DEF_SAT_U_MUL_FMT_1_WRAP(NT, WT)

/* { dg-final { scan-tree-dump-times ".SAT_MUL" 1 "optimized" } } */
/* { dg-final { scan-assembler-not "\.L\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "mulhu" 1 } } */
